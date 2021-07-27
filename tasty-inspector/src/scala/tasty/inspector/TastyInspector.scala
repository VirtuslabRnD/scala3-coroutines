package scala.tasty.inspector

import scala.quoted._
import scala.quoted.runtime.impl.QuotesImpl

import dotty.tools.dotc.Compiler
import dotty.tools.dotc.Driver
import dotty.tools.dotc.Run
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Mode
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.fromtasty._
import dotty.tools.dotc.util.ClasspathFromClassloader
import dotty.tools.dotc.CompilationUnit
import dotty.tools.unsupported
import dotty.tools.dotc.report

import java.io.File.pathSeparator

object TastyInspector:

  /** Load and process TASTy files using TASTy reflect
   *
   *  @param tastyFiles List of paths of `.tasty` files
   */
  def inspectTastyFiles(tastyFiles: List[String])(inspector: Inspector): Boolean =
    inspectAllTastyFiles(tastyFiles, Nil, Nil)(inspector)

  /** Load and process TASTy files in a `jar` file using TASTy reflect
   *
   *  @param jars Path of `.jar` file
   */
  def inspectTastyFilesInJar(jar: String)(inspector: Inspector): Boolean =
    inspectAllTastyFiles(Nil, List(jar), Nil)(inspector)

  /** Load and process TASTy files using TASTy reflect
   *
   *  @param tastyFiles List of paths of `.tasty` files
   *  @param jars List of path of `.jar` files
   *  @param dependenciesClasspath Classpath with extra dependencies needed to load class in the `.tasty` files
   */
  def inspectAllTastyFiles(tastyFiles: List[String], jars: List[String], dependenciesClasspath: List[String])(inspector: Inspector): Boolean =
    def checkFile(fileName: String, ext: String): Unit =
      val file = dotty.tools.io.Path(fileName)
      if file.extension != ext then
        throw new IllegalArgumentException(s"File extension is not `.$ext`: $file")
      else if !file.exists then
        throw new IllegalArgumentException(s"File not found: ${file.toAbsolute}")
    tastyFiles.foreach(checkFile(_, "tasty"))
    jars.foreach(checkFile(_, "jar"))
    val files = tastyFiles ::: jars
    files.nonEmpty && inspectFiles(dependenciesClasspath, files)(inspector)

  private def inspectorDriver(inspector: Inspector) =
    class InspectorDriver extends Driver:
      override protected def newCompiler(implicit ctx: Context): Compiler = new TastyFromClass

    class TastyInspectorPhase extends Phase:
      override def phaseName: String = "tastyInspector"

      override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] =
        val quotesImpl = QuotesImpl()
        class TastyImpl(val path: String, val ast: quotesImpl.reflect.Tree) extends Tasty[quotesImpl.type] {
          val quotes = quotesImpl
        }
        val tastys = units.map(unit => new TastyImpl(unit.source.path , unit.tpdTree.asInstanceOf[quotesImpl.reflect.Tree]))
        inspector.inspect(using quotesImpl)(tastys)
        units

      override def run(implicit ctx: Context): Unit = unsupported("run")
    end TastyInspectorPhase

    class TastyFromClass extends TASTYCompiler:

      override protected def frontendPhases: List[List[Phase]] =
        List(new ReadTasty) :: // Load classes from tasty
        Nil

      override protected def picklerPhases: List[List[Phase]] = Nil

      override protected def transformPhases: List[List[Phase]] = Nil

      override protected def backendPhases: List[List[Phase]] =
        List(new TastyInspectorPhase) ::  // Perform a callback for each compilation unit
        Nil

      override def newRun(implicit ctx: Context): Run =
        reset()
        val ctx2 = ctx.fresh
            .addMode(Mode.ReadPositions)
            .setSetting(ctx.settings.YreadComments, true)
        new TASTYRun(this, ctx2)

    new InspectorDriver

  private def inspectorArgs(classpath: List[String], classes: List[String]): Array[String] =
    val currentClasspath = ClasspathFromClassloader(getClass.getClassLoader)
    val fullClasspath = (classpath :+ currentClasspath).mkString(pathSeparator)
    ("-from-tasty" :: "-Yretain-trees" :: "-classpath" :: fullClasspath :: classes).toArray


  private def inspectFiles(classpath: List[String], classes: List[String])(inspector: Inspector): Boolean =
    if (classes.isEmpty)
      throw new IllegalArgumentException("Parameter classes should no be empty")

    val reporter = inspectorDriver(inspector).process(inspectorArgs(classpath, classes))
    reporter.hasErrors

  end inspectFiles


end TastyInspector
