package dotty.tools
package repl

import java.io.{ StringWriter, PrintWriter }
import java.lang.{ ClassLoader, ExceptionInInitializerError }
import java.lang.reflect.InvocationTargetException

import dotc.ast.tpd
import dotc.core.Contexts._
import dotc.core.Denotations.Denotation
import dotc.core.Flags
import dotc.core.Flags._
import dotc.core.Symbols.{Symbol, defn}
import dotc.core.StdNames.{nme, str}
import dotc.core.NameOps._
import dotc.printing.ReplPrinter
import dotc.reporting.{MessageRendering, Message, Diagnostic}
import dotc.util.SourcePosition

/** This rendering object uses `ClassLoader`s to accomplish crossing the 4th
 *  wall (i.e. fetching back values from the compiled class files put into a
 *  specific class loader capable of loading from memory) and rendering them.
 *
 *  @pre this object should be paired with a compiler session, i.e. when
 *       `ReplDriver#resetToInitial` is called, the accompanying instance of
 *       `Rendering` is no longer valid.
 */
private[repl] class Rendering(parentClassLoader: Option[ClassLoader] = None) {

  import Rendering._

  private val MaxStringElements: Int = 1000  // no need to mkString billions of elements

  /** A `MessageRenderer` for the REPL without file positions */
  private val messageRenderer = new MessageRendering {
    override def posStr(pos: SourcePosition, diagnosticLevel: String, message: Message)(using Context): String = ""
  }

  private var myClassLoader: ClassLoader = _

  private var myReplStringOf: Object => String = _


  /** Class loader used to load compiled code */
  private[repl] def classLoader()(using Context) =
    if (myClassLoader != null) myClassLoader
    else {
      val parent = parentClassLoader.getOrElse {
        val compilerClasspath = ctx.platform.classPath(using ctx).asURLs
        // We can't use the system classloader as a parent because it would
        // pollute the user classpath with everything passed to the JVM
        // `-classpath`. We can't use `null` as a parent either because on Java
        // 9+ that's the bootstrap classloader which doesn't contain modules
        // like `java.sql`, so we use the parent of the system classloader,
        // which should correspond to the platform classloader on Java 9+.
        val baseClassLoader = ClassLoader.getSystemClassLoader.getParent
        new java.net.URLClassLoader(compilerClasspath.toArray, baseClassLoader)
      }

      myClassLoader = new AbstractFileClassLoader(ctx.settings.outputDir.value, parent)
      myReplStringOf = {
        // We need to use the ScalaRunTime class coming from the scala-library
        // on the user classpath, and not the one available in the current
        // classloader, so we use reflection instead of simply calling
        // `ScalaRunTime.replStringOf`. Probe for new API without extraneous newlines.
        // For old API, try to clean up extraneous newlines by stripping suffix and maybe prefix newline.
        val scalaRuntime = Class.forName("scala.runtime.ScalaRunTime", true, myClassLoader)
        val renderer = "stringOf"  // was: replStringOf
        try {
          val meth = scalaRuntime.getMethod(renderer, classOf[Object], classOf[Int], classOf[Boolean])
          val truly = java.lang.Boolean.TRUE

          (value: Object) => meth.invoke(null, value, Integer.valueOf(MaxStringElements), truly).asInstanceOf[String]
        } catch {
          case _: NoSuchMethodException =>
            val meth = scalaRuntime.getMethod(renderer, classOf[Object], classOf[Int])

            (value: Object) => meth.invoke(null, value, Integer.valueOf(MaxStringElements)).asInstanceOf[String]
        }
      }
      myClassLoader
    }

  /** Used to elide long output in replStringOf.
   *
   * TODO: Perhaps implement setting scala.repl.maxprintstring as in Scala 2, but
   * then this bug will surface, so perhaps better not?
   * https://github.com/scala/bug/issues/12337
   */
  private[repl] def truncate(str: String): String = {
    val showTruncated = " ... large output truncated, print value to show all"
    val ncp = str.codePointCount(0, str.length) // to not cut inside code point
    if ncp <= MaxStringElements then str
    else str.substring(0, str.offsetByCodePoints(0, MaxStringElements - 1)) + showTruncated
  }

  /** Return a String representation of a value we got from `classLoader()`. */
  private[repl] def replStringOf(value: Object)(using Context): String = {
    assert(myReplStringOf != null,
      "replStringOf should only be called on values creating using `classLoader()`, but `classLoader()` has not been called so far")
    val res = myReplStringOf(value)
    if res == null then "null // non-null reference has null-valued toString" else truncate(res)
  }

  /** Load the value of the symbol using reflection.
   *
   *  Calling this method evaluates the expression using reflection
   */
  private def valueOf(sym: Symbol)(using Context): Option[String] = {
    val objectName = sym.owner.fullName.encode.toString.stripSuffix("$")
    val resObj: Class[?] = Class.forName(objectName, true, classLoader())
    val value =
      resObj
        .getDeclaredMethods.find(_.getName == sym.name.encode.toString)
        .map(_.invoke(null))
    val string = value.map(replStringOf(_))
    if (!sym.is(Flags.Method) && sym.info == defn.UnitType)
      None
    else
      string.map { s =>
        if (s.startsWith(REPL_WRAPPER_NAME_PREFIX))
          s.drop(REPL_WRAPPER_NAME_PREFIX.length).dropWhile(c => c.isDigit || c == '$')
        else
          s
      }
  }

  /** Formats errors using the `messageRenderer` */
  def formatError(dia: Diagnostic)(implicit state: State): Diagnostic =
    new Diagnostic(
      messageRenderer.messageAndPos(dia.msg, dia.pos, messageRenderer.diagnosticLevel(dia))(using state.context),
      dia.pos,
      dia.level
    )

  def renderTypeDef(d: Denotation)(using Context): Diagnostic =
    infoDiagnostic("// defined " ++ d.symbol.showUser, d)

  def renderTypeAlias(d: Denotation)(using Context): Diagnostic =
    infoDiagnostic("// defined alias " ++ d.symbol.showUser, d)

  /** Render method definition result */
  def renderMethod(d: Denotation)(using Context): Diagnostic =
    infoDiagnostic(d.symbol.showUser, d)

  /** Render value definition result */
  def renderVal(d: Denotation)(using Context): Option[Diagnostic] =
    val dcl = d.symbol.showUser
    def msg(s: String) = infoDiagnostic(s, d)
    try
      if (d.symbol.is(Flags.Lazy)) Some(msg(dcl))
      else valueOf(d.symbol).map(value => msg(s"$dcl = $value"))
    catch case e: InvocationTargetException => Some(msg(renderError(e, d)))
  end renderVal

  /** Force module initialization in the absence of members. */
  def forceModule(sym: Symbol)(using Context): Seq[Diagnostic] =
    def load() =
      val objectName = sym.fullName.encode.toString
      Class.forName(objectName, true, classLoader())
      Nil
    try load() catch case e: ExceptionInInitializerError => List(infoDiagnostic(renderError(e, sym.denot), sym.denot))

  /** Render the stack trace of the underlying exception. */
  private def renderError(ite: InvocationTargetException | ExceptionInInitializerError, d: Denotation)(using Context): String =
    import dotty.tools.dotc.util.StackTraceOps._
    val cause = ite.getCause match
      case e: ExceptionInInitializerError => e.getCause
      case e => e
    def isWrapperCode(ste: StackTraceElement) =
      ste.getClassName == d.symbol.owner.name.show
      && (ste.getMethodName == nme.STATIC_CONSTRUCTOR.show || ste.getMethodName == nme.CONSTRUCTOR.show)

    cause.formatStackTracePrefix(!isWrapperCode(_))
  end renderError

  private def infoDiagnostic(msg: String, d: Denotation)(using Context): Diagnostic =
    new Diagnostic.Info(msg, d.symbol.sourcePos)

}

object Rendering {
  final val REPL_WRAPPER_NAME_PREFIX = s"${nme.REPL_PACKAGE}.${str.REPL_SESSION_LINE}"

  extension (s: Symbol)
    def showUser(using Context): String = {
      val printer = new ReplPrinter(ctx)
      val text = printer.dclText(s)
      text.mkString(ctx.settings.pageWidth.value, ctx.settings.printLines.value)
    }

}
