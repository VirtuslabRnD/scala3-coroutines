package dotty.tools.dotc
package transform

import core._
import StdNames.nme
import Types._
import transform.MegaPhase._
import ast.Trees._
import Flags._
import Contexts._
import Symbols._
import Constants._
import Decorators._
import Denotations._, SymDenotations._
import TypeErasure.erasure
import DenotTransformers._

object ElimRepeated {
  val name: String = "elimRepeated"
}

/** A transformer that eliminates repeated parameters (T*) from all types, replacing
 *  them with Seq or Array types and adapting repeated arguments to conform to
 *  the transformed type if needed.
 */
class ElimRepeated extends MiniPhase with InfoTransformer { thisPhase =>
  import ast.tpd._

  override def phaseName: String = ElimRepeated.name

  override def changesMembers: Boolean = true // the phase adds vararg forwarders

  def transformInfo(tp: Type, sym: Symbol)(using Context): Type =
    elimRepeated(tp, sym.is(JavaDefined))

  /** Create forwarder symbols for the methods that are annotated
   *  with `@varargs` or that override java varargs.
   *
   *  The definitions (DefDef) for these symbols are created by transformDefDef.
   */
  override def transform(ref: SingleDenotation)(using Context): SingleDenotation =
    def transformVarArgs(sym: Symbol, isJavaVarargsOverride: Boolean): Unit =
      val hasAnnotation = hasVarargsAnnotation(sym)
      val hasRepeatedParam = hasRepeatedParams(sym)
      if hasRepeatedParam then
        val parentHasAnnotation = parentHasVarargsAnnotation(sym)
        if isJavaVarargsOverride || hasAnnotation || parentHasAnnotation then
          // java varargs are more restrictive than scala's
          // see https://github.com/scala/bug/issues/11714
          val validJava = isValidJavaVarArgs(sym.info)
          if !validJava then
            report.error("""To generate java-compatible varargs:
                      |  - there must be a single repeated parameter
                      |  - it must be the last argument in the last parameter list
                      |""".stripMargin,
              sym.sourcePos)
          else
            addVarArgsForwarder(sym, isJavaVarargsOverride, hasAnnotation, parentHasAnnotation)
      else if hasAnnotation then
        report.error("A method without repeated parameters cannot be annotated with @varargs", sym.sourcePos)
    end

    super.transform(ref) match
      case ref1: SymDenotation if ref1.is(Method, butNot = JavaDefined) =>
        val sym = ref1.symbol
        val isJavaVarargsOverride = (ref1 ne ref) && overridesJava(sym)
        transformVarArgs(sym, isJavaVarargsOverride)
        if isJavaVarargsOverride then
          // This method won't override the corresponding Java method at the end of this phase,
          // only the forwarder added by `addVarArgsForwarder` will.
          ref1.copySymDenotation(initFlags = ref1.flags &~ Override)
        else
          ref1
      case ref1 =>
        ref1

  override def infoMayChange(sym: Symbol)(using Context): Boolean = sym.is(Method)

  private def overridesJava(sym: Symbol)(using Context) = sym.allOverriddenSymbols.exists(_.is(JavaDefined))

  private def hasVarargsAnnotation(sym: Symbol)(using Context) = sym.hasAnnotation(defn.VarargsAnnot)

  private def parentHasVarargsAnnotation(sym: Symbol)(using Context) = sym.allOverriddenSymbols.exists(hasVarargsAnnotation)

  private def isVarargsMethod(sym: Symbol)(using Context) =
    hasVarargsAnnotation(sym) ||
      hasRepeatedParams(sym) &&
      (sym.allOverriddenSymbols.exists(s => s.is(JavaDefined) || hasVarargsAnnotation(s)))

  /** Eliminate repeated parameters from method types. */
  private def elimRepeated(tp: Type, isJava: Boolean)(using Context): Type = tp.stripTypeVar match
    case tp @ MethodTpe(paramNames, paramTypes, resultType) =>
      val resultType1 = elimRepeated(resultType, isJava)
      val paramTypes1 =
        val lastIdx = paramTypes.length - 1
        if lastIdx >= 0 then
          val last = paramTypes(lastIdx)
          if last.isRepeatedParam then
            // We need to be careful when handling Java repeated parameters
            // of the form `Object...` or `T...` where `T` is unbounded:
            // in both cases, `Object` will have been translated to `FromJavaObject`
            // to allow passing primitives as repeated arguments, but we can't
            // pass a primitive array as argument to such a method since the
            // parameter will be erased to `Object[]`. To handle this correctly we
            // drop usage of `FromJavaObject` as an element type here, the
            // tree transformer of this phase is then responsible for handling
            // mismatches by emitting the correct adaptation (cf `adaptToArray`).
            // See also the documentation of `FromJavaObjectSymbol`.
            val last1 =
              if isJava && last.elemType.isFromJavaObject then
                defn.ArrayOf(TypeBounds.upper(defn.ObjectType))
              else
                last.translateFromRepeated(toArray = isJava)
            paramTypes.updated(lastIdx, last1)
          else paramTypes
        else paramTypes
      tp.derivedLambdaType(paramNames, paramTypes1, resultType1)
    case tp: PolyType =>
      tp.derivedLambdaType(tp.paramNames, tp.paramInfos, elimRepeated(tp.resultType, isJava))
    case tp =>
      tp

  override def transformApply(tree: Apply)(using Context): Tree =
    val args = tree.args.mapConserve {
      case arg: Typed if isWildcardStarArg(arg) =>
        val isJavaDefined = tree.fun.symbol.is(JavaDefined)
        val tpe = arg.expr.tpe
        if isJavaDefined then
          val pt = tree.fun.tpe.widen.firstParamTypes.last
          adaptToArray(arg.expr, pt.elemType.bounds.hi)
        else if tpe.derivesFrom(defn.ArrayClass) then
          arrayToSeq(arg.expr)
        else
          arg.expr
      case arg => arg
    }
    cpy.Apply(tree)(tree.fun, args)

  /** Convert sequence argument to Java array */
  private def seqToArray(tree: Tree)(using Context): Tree = tree match
    case SeqLiteral(elems, elemtpt) =>
      JavaSeqLiteral(elems, elemtpt)
    case _ =>
      val elemType = tree.tpe.elemType
      var elemClass = erasure(elemType).classSymbol
      if defn.NotRuntimeClasses.contains(elemClass) then
        elemClass = defn.ObjectClass
      end if
      ref(defn.DottyArraysModule)
        .select(nme.seqToArray)
        .appliedToType(elemType)
        .appliedTo(tree, clsOf(elemClass.typeRef))

  /** Adapt a Seq or Array tree to be a subtype of `Array[_ <: $elemPt]`.
   *
   *  @pre `elemPt` must either be a super type of the argument element type or `Object`.
   *        The special handling of `Object` is required to deal with the translation
   *        of generic Java varargs in `elimRepeated`.
   */
  private def adaptToArray(tree: Tree, elemPt: Type)(implicit ctx: Context): Tree =
    val elemTp = tree.tpe.elemType
    val elemTpMatches = elemTp <:< elemPt
    val treeIsArray = tree.tpe.derivesFrom(defn.ArrayClass)
    if elemTpMatches && treeIsArray then
      tree // No adaptation necessary
    else tree match
      case SeqLiteral(elems, elemtpt) =>
        // By the precondition, we only have mismatches if elemPt is Object, in
        // that case we use `FromJavaObject` as the element type to allow the
        // sequence literal to typecheck no matter the types of the elements,
        // Erasure will take care of any necessary boxing (see documentation
        // of `FromJavaObjectSymbol` for more information).
        val adaptedElemTpt = if elemTpMatches then elemtpt else TypeTree(defn.FromJavaObjectType)
        JavaSeqLiteral(elems, adaptedElemTpt).withSpan(tree.span)
      case _ =>
        if treeIsArray then
          // Convert an Array[T] to an Array[Object]
          ref(defn.ScalaRuntime_toObjectArray)
            .appliedTo(tree)
        else if elemTpMatches then
          // Convert a Seq[T] to an Array[$elemPt]
          ref(defn.DottyArraysModule)
            .select(nme.seqToArray)
            .appliedToType(elemPt)
            .appliedTo(tree, clsOf(elemPt))
        else
          // Convert a Seq[T] to an Array[Object]
          ref(defn.ScalaRuntime_toArray)
            .appliedToType(elemTp)
            .appliedTo(tree)

  /** Convert an Array into a scala.Seq */
  private def arrayToSeq(tree: Tree)(using Context): Tree =
    wrapArray(tree, tree.tpe.elemType)

  /** Generate the method definitions for the varargs forwarders created in transform */
  override def transformDefDef(tree: DefDef)(using Context): Tree =
    // If transform reported an error, don't go further
    if ctx.reporter.hasErrors then
      return tree

    val sym = tree.symbol
    val isVarArgs = atPhase(thisPhase)(isVarargsMethod(sym))
    if isVarArgs then
      // Get the symbol generated in transform
      val forwarderType = atPhase(thisPhase)(toJavaVarArgs(sym.info))
      val forwarderSym = currentClass.info.decl(sym.name).alternatives
        .find(_.info.matches(forwarderType))
        .get
        .symbol.asTerm
      // Generate the method
      val forwarderDef = DefDef(forwarderSym, prefss => {
        val init :+ (last :+ vararg) = prefss
        // Can't call `.argTypes` here because the underlying array type is of the
        // form `Array[? <: SomeType]`, so we need `.argInfos` to get the `TypeBounds`.
        val elemtp = vararg.tpe.widen.argInfos.head
        ref(sym.termRef)
          .appliedToArgss(init)
          .appliedToTermArgs(last :+ wrapArray(vararg, elemtp))
        })
      Thicket(tree, forwarderDef)
    else
      tree

  /** Is there a repeated parameter in some parameter list? */
  private def hasRepeatedParams(sym: Symbol)(using Context): Boolean =
    sym.info.paramInfoss.nestedExists(_.isRepeatedParam)

  /** Is this the type of a method that has a repeated parameter type as
   *  its last parameter in the last parameter list?
   */
  private def isValidJavaVarArgs(tp: Type)(using Context): Boolean = tp match
    case mt: MethodType =>
      val initp :+ lastp = mt.paramInfoss
      initp.forall(_.forall(!_.isRepeatedParam)) &&
      lastp.nonEmpty &&
      lastp.init.forall(!_.isRepeatedParam) &&
      lastp.last.isRepeatedParam
    case pt: PolyType =>
      isValidJavaVarArgs(pt.resultType)
    case _ =>
      throw new Exception("Match error in @varargs checks. This should not happen, please open an issue " + tp)

  /** Add the symbol of a Java varargs forwarder to the scope.
   *  It retains all the flags of the original method.
   *
   *  @param original the original method symbol
   *  @param isBridge true if we are generating a "bridge" (synthetic override forwarder)
   *  @param hasAnnotation true if the method is annotated with `@varargs`
   *  @param parentHasAnnotation true if the method overrides a method that is annotated with `@varargs`
   *
   *  A forwarder is necessary because the following holds:
   *    - the varargs in `original` will change from `RepeatedParam[T]` to `Seq[T]` after this phase
   *    - _but_ the callers of the method expect its varargs to be changed to `Array[? <: T]`
   *  The solution is to add a method that converts its argument from `Array[? <: T]` to `Seq[T]` and
   *  forwards it to the original method.
   */
  private def addVarArgsForwarder(original: Symbol, isBridge: Boolean, hasAnnotation: Boolean, parentHasAnnotation: Boolean)(using Context): Unit =
    val owner = original.owner
    if !owner.isClass then
      report.error("inner methods cannot be annotated with @varargs", original.sourcePos)
      return

    val classInfo = owner.info

    // For simplicity we always set the varargs flag,
    // although it's not strictly necessary for overrides.
    val flags = original.flags | JavaVarargs

    // The java-compatible forwarder symbol
    val forwarder =
      original.copy(
        flags =
          if isBridge then flags | Artifact
          else if hasAnnotation && !parentHasAnnotation then flags &~ Override
          else flags,
        info = toJavaVarArgs(original.info)
      ).asTerm

    // Find methods that would conflict with the forwarder if the latter existed.
    // This needs to be done at thisPhase so that parent @varargs don't conflict.
    val conflicts =
      classInfo.member(original.name).altsWith { s =>
        s.matches(forwarder) && !(isBridge && s.is(JavaDefined))
      }
    conflicts match
      case conflict :: _ =>
        val src =
          if hasAnnotation then "@varargs"
          else if isBridge then "overriding a java varargs method"
          else "@varargs (on overridden method)"
        report.error(s"$src produces a forwarder method that conflicts with ${conflict.showDcl}", original.srcPos)
      case Nil =>
        forwarder.enteredAfter(thisPhase)

  /** Convert type from Scala to Java varargs method */
  private def toJavaVarArgs(tp: Type)(using Context): Type = tp match
    case tp: PolyType =>
      tp.derivedLambdaType(tp.paramNames, tp.paramInfos, toJavaVarArgs(tp.resultType))
    case tp: MethodType =>
      tp.resultType match
        case m: MethodType => // multiple param lists
          tp.derivedLambdaType(tp.paramNames, tp.paramInfos, toJavaVarArgs(m))
        case _ =>
          val init :+ last = tp.paramInfos
          val vararg = varargArrayType(last)
          tp.derivedLambdaType(tp.paramNames, init :+ vararg, tp.resultType)

  /** Translate a repeated type T* to an `Array[? <: Upper]`
   *  such that it is compatible with java varargs.
   *
   *  When necessary we set `Upper = T & AnyRef`
   *  to prevent the erasure of `Array[? <: Upper]` to Object,
   *  which would break the varargs from Java.
   */
  private def varargArrayType(tp: Type)(using Context): Type =
    val array = tp.translateFromRepeated(toArray = true) // Array[? <: T]
    val element = array.elemType.hiBound // T

    if element <:< defn.AnyRefType || element.typeSymbol.isPrimitiveValueClass then array
    else defn.ArrayOf(TypeBounds.upper(AndType(element, defn.AnyRefType))) // Array[? <: T & AnyRef]
}
