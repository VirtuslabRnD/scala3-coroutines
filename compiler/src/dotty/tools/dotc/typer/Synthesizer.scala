package dotty.tools
package dotc
package typer

import core._
import util.Spans.Span
import Contexts._
import Types._, Flags._, Symbols._, Types._, Names._, StdNames._, Constants._
import TypeErasure.{erasure, hasStableErasure}
import Decorators._
import ProtoTypes._
import Inferencing.{fullyDefinedType, isFullyDefined}
import ast.untpd
import transform.SymUtils._
import transform.TypeUtils._
import transform.SyntheticMembers._
import util.Property
import annotation.{tailrec, constructorOnly}

/** Synthesize terms for special classes */
class Synthesizer(typer: Typer)(using @constructorOnly c: Context):
  import ast.tpd._

  /** Handlers to synthesize implicits for special types */
  type SpecialHandler = (Type, Span) => Context ?=> Tree
  private type SpecialHandlers = List[(ClassSymbol, SpecialHandler)]

  val synthesizedClassTag: SpecialHandler = (formal, span) =>
    formal.argInfos match
      case arg :: Nil =>
        fullyDefinedType(arg, "ClassTag argument", span) match
          case defn.ArrayOf(elemTp) =>
            val etag = typer.inferImplicitArg(defn.ClassTagClass.typeRef.appliedTo(elemTp), span)
            if etag.tpe.isError then EmptyTree else etag.select(nme.wrap)
          case tp if hasStableErasure(tp) && !defn.isBottomClassAfterErasure(tp.typeSymbol) =>
            val sym = tp.typeSymbol
            val classTag = ref(defn.ClassTagModule)
            val tag =
              if defn.SpecialClassTagClasses.contains(sym) then
                classTag.select(sym.name.toTermName)
              else
                val clsOfType = erasure(tp) match
                  case JavaArrayType(elemType) => defn.ArrayOf(elemType)
                  case etp => etp
                classTag.select(nme.apply).appliedToType(tp).appliedTo(clsOf(clsOfType))
            tag.withSpan(span)
          case tp => EmptyTree
      case _ => EmptyTree
  end synthesizedClassTag

  val synthesizedTypeTest: SpecialHandler =
    (formal, span) => formal.argInfos match {
      case arg1 :: arg2 :: Nil if !defn.isBottomClass(arg2.typeSymbol) =>
        val tp1 = fullyDefinedType(arg1, "TypeTest argument", span)
        val tp2 = fullyDefinedType(arg2, "TypeTest argument", span)
        val sym2 = tp2.typeSymbol
        if tp1 <:< tp2 then
          // optimization when we know the typetest will always succeed
          ref(defn.TypeTestModule_identity).appliedToType(tp2).withSpan(span)
        else if sym2 == defn.AnyValClass || sym2 == defn.AnyRefAlias || sym2 == defn.ObjectClass then
          EmptyTree
        else
          // Generate SAM: (s: <tp1>) => if s.isInstanceOf[<tp2>] then Some(s.asInstanceOf[s.type & <tp2>]) else None
          def body(args: List[Tree]): Tree = {
            val arg :: Nil = args
            val t = arg.tpe & tp2
            If(
              arg.isInstance(tp2),
              ref(defn.SomeClass.companionModule.termRef).select(nme.apply)
                .appliedToType(t)
                .appliedTo(arg.select(nme.asInstanceOf_).appliedToType(t)),
              ref(defn.NoneModule))
          }
          val tpe = MethodType(List(nme.s))(_ => List(tp1), mth => defn.OptionClass.typeRef.appliedTo(mth.newParamRef(0) & tp2))
          val meth = newSymbol(ctx.owner, nme.ANON_FUN, Synthetic | Method, tpe, coord = span)
          val typeTestType = defn.TypeTestClass.typeRef.appliedTo(List(tp1, tp2))
          Closure(meth, tss => body(tss.head).changeOwner(ctx.owner, meth), targetType = typeTestType).withSpan(span)
      case _ =>
        EmptyTree
    }
  end synthesizedTypeTest

  /** If `formal` is of the form CanEqual[T, U], try to synthesize an
    *  `CanEqual.canEqualAny[T, U]` as solution.
    */
  val synthesizedCanEqual: SpecialHandler = (formal, span) =>

    /** Is there an `CanEqual[T, T]` instance, assuming -strictEquality? */
    def hasEq(tp: Type)(using Context): Boolean =
      val inst = typer.inferImplicitArg(defn.CanEqualClass.typeRef.appliedTo(tp, tp), span)
      !inst.isEmpty && !inst.tpe.isError

    /** Can we assume the canEqualAny instance for `tp1`, `tp2`?
      *  This is the case if assumedCanEqual(tp1, tp2), or
      *  one of `tp1`, `tp2` has a reflexive `CanEqual` instance.
      */
    def validEqAnyArgs(tp1: Type, tp2: Type)(using Context) =
      typer.assumedCanEqual(tp1, tp2)
       || withMode(Mode.StrictEquality) {
            !hasEq(tp1) && !hasEq(tp2)
          }

    /** Is an `CanEqual[cls1, cls2]` instance assumed for predefined classes `cls1`, cls2`? */
    def canComparePredefinedClasses(cls1: ClassSymbol, cls2: ClassSymbol): Boolean =

      def cmpWithBoxed(cls1: ClassSymbol, cls2: ClassSymbol) =
        cls2 == defn.boxedType(cls1.typeRef).symbol
        || cls1.isNumericValueClass && cls2.derivesFrom(defn.BoxedNumberClass)

      if cls1.isPrimitiveValueClass then
        if cls2.isPrimitiveValueClass then
          cls1 == cls2 || cls1.isNumericValueClass && cls2.isNumericValueClass
        else
          cmpWithBoxed(cls1, cls2)
      else if cls2.isPrimitiveValueClass then
        cmpWithBoxed(cls2, cls1)
      else if ctx.mode.is(Mode.SafeNulls) then
        // If explicit nulls is enabled, and unsafeNulls is not enabled,
        // we want to disallow comparison between Object and Null.
        // If we have to check whether a variable with a non-nullable type has null value
        // (for example, a NotNull java method returns null for some reasons),
        // we can still cast it to a nullable type then compare its value.
        //
        // Example:
        // val x: String = null.asInstanceOf[String]
        // if (x == null) {} // error: x is non-nullable
        // if (x.asInstanceOf[String|Null] == null) {} // ok
        cls1 == defn.NullClass && cls1 == cls2
      else if cls1 == defn.NullClass then
        cls1 == cls2 || cls2.derivesFrom(defn.ObjectClass)
      else if cls2 == defn.NullClass then
        cls1.derivesFrom(defn.ObjectClass)
      else
        false
    end canComparePredefinedClasses

    /** Some simulated `CanEqual` instances for predefined types. It's more efficient
      *  to do this directly instead of setting up a lot of `CanEqual` instances to
      *  interpret.
      */
    def canComparePredefined(tp1: Type, tp2: Type) =
      tp1.classSymbols.exists(cls1 =>
        tp2.classSymbols.exists(cls2 =>
          canComparePredefinedClasses(cls1, cls2)))

    formal.argTypes match
      case args @ (arg1 :: arg2 :: Nil) =>
        List(arg1, arg2).foreach(fullyDefinedType(_, "eq argument", span))
        if canComparePredefined(arg1, arg2)
            || !Implicits.strictEquality && explore(validEqAnyArgs(arg1, arg2))
        then ref(defn.CanEqual_canEqualAny).appliedToTypes(args).withSpan(span)
        else EmptyTree
      case _ => EmptyTree
  end synthesizedCanEqual

  /** Creates a tree that will produce a ValueOf instance for the requested type.
   * An EmptyTree is returned if materialization fails.
   */
  val synthesizedValueOf: SpecialHandler = (formal, span) =>

    def success(t: Tree) =
      New(defn.ValueOfClass.typeRef.appliedTo(t.tpe), t :: Nil).withSpan(span)
    formal.argInfos match
      case arg :: Nil =>
        fullyDefinedType(arg.dealias, "ValueOf argument", span).normalized match
          case ConstantType(c: Constant) =>
            success(Literal(c))
          case tp: TypeRef if tp.isRef(defn.UnitClass) =>
            success(Literal(Constant(())))
          case n: TermRef =>
            success(ref(n))
          case tp =>
            EmptyTree
      case _ =>
        EmptyTree
  end synthesizedValueOf

  /** Create an anonymous class `new Object { type MirroredMonoType = ... }`
   *  and mark it with given attachment so that it is made into a mirror at PostTyper.
   */
  private def anonymousMirror(monoType: Type, attachment: Property.StickyKey[Unit], span: Span)(using Context) =
    if ctx.isAfterTyper then ctx.compilationUnit.needsMirrorSupport = true
    val monoTypeDef = untpd.TypeDef(tpnme.MirroredMonoType, untpd.TypeTree(monoType))
    val newImpl = untpd.Template(
      constr = untpd.emptyConstructor,
      parents = untpd.TypeTree(defn.ObjectType) :: Nil,
      derived = Nil,
      self = EmptyValDef,
      body = monoTypeDef :: Nil
    ).withAttachment(attachment, ())
    typer.typed(untpd.New(newImpl).withSpan(span))

  /** The mirror type
   *
   *     <parent> {
   *       MirroredMonoType = <monoType>
   *       MirroredType = <mirroredType>
   *       MirroredLabel = <label> }
   *     }
   */
  private def mirrorCore(parentClass: ClassSymbol, monoType: Type, mirroredType: Type, label: Name, formal: Type)(using Context) =
    formal & parentClass.typeRef
      .refinedWith(tpnme.MirroredMonoType, TypeAlias(monoType))
      .refinedWith(tpnme.MirroredType, TypeAlias(mirroredType))
      .refinedWith(tpnme.MirroredLabel, TypeAlias(ConstantType(Constant(label.toString))))

  /** A path referencing the companion of class type `clsType` */
  private def companionPath(clsType: Type, span: Span)(using Context) =
    val ref = pathFor(clsType.companionRef)
    assert(ref.symbol.is(Module) && (clsType.classSymbol.is(ModuleClass) || (ref.symbol.companionClass == clsType.classSymbol)))
    ref.withSpan(span)

  private def checkFormal(formal: Type)(using Context): Boolean =
    @tailrec
    def loop(tp: Type): Boolean = tp match
      case RefinedType(parent, _, _: TypeBounds) => loop(parent)
      case RefinedType(_, _, _) => false
      case _ => true
    loop(formal)

  private def checkRefinement(formal: Type, name: TypeName, expected: Type, span: Span)(using Context): Unit =
    val actual = formal.lookupRefined(name)
    if actual.exists && !(expected =:= actual)
    then report.error(
      em"$name mismatch, expected: $expected, found: $actual.", ctx.source.atSpan(span))

  private def mkMirroredMonoType(mirroredType: HKTypeLambda)(using Context): Type =
    val monoMap = new TypeMap:
      def apply(t: Type) = t match
        case TypeParamRef(lambda, n) if lambda eq mirroredType => mirroredType.paramInfos(n)
        case t => mapOver(t)
    monoMap(mirroredType.resultType)

  private def productMirror(mirroredType: Type, formal: Type, span: Span)(using Context): Tree =
    mirroredType match
      case AndType(tp1, tp2) =>
        productMirror(tp1, formal, span).orElse(productMirror(tp2, formal, span))
      case _ =>
        if mirroredType.termSymbol.is(CaseVal) then
          val module = mirroredType.termSymbol
          val modulePath = pathFor(mirroredType).withSpan(span)
          if module.info.classSymbol.is(Scala2x) then
            val mirrorType = mirrorCore(defn.Mirror_SingletonProxyClass, mirroredType, mirroredType, module.name, formal)
            val mirrorRef = New(defn.Mirror_SingletonProxyClass.typeRef, modulePath :: Nil)
            mirrorRef.cast(mirrorType)
          else
            val mirrorType = mirrorCore(defn.Mirror_SingletonClass, mirroredType, mirroredType, module.name, formal)
            modulePath.cast(mirrorType)
        else if mirroredType.classSymbol.isGenericProduct then
          val cls = mirroredType.classSymbol
          val accessors = cls.caseAccessors.filterNot(_.isAllOf(PrivateLocal))
          val elemLabels = accessors.map(acc => ConstantType(Constant(acc.name.toString)))
          val (monoType, elemsType) = mirroredType match
            case mirroredType: HKTypeLambda =>
              def accessorType(acc: Symbol) =
                if cls.typeParams.hasSameLengthAs(mirroredType.paramRefs) then
                  acc.info.subst(cls.typeParams, mirroredType.paramRefs)
                else
                  acc.info
              val elems =
                mirroredType.derivedLambdaType(
                  resType = TypeOps.nestedPairs(accessors.map(accessorType))
                )
              (mkMirroredMonoType(mirroredType), elems)
            case _ =>
              val elems = TypeOps.nestedPairs(accessors.map(mirroredType.memberInfo(_).widenExpr))
              (mirroredType, elems)
          val elemsLabels = TypeOps.nestedPairs(elemLabels)
          checkRefinement(formal, tpnme.MirroredElemTypes, elemsType, span)
          checkRefinement(formal, tpnme.MirroredElemLabels, elemsLabels, span)
          val mirrorType =
            mirrorCore(defn.Mirror_ProductClass, monoType, mirroredType, cls.name, formal)
              .refinedWith(tpnme.MirroredElemTypes, TypeAlias(elemsType))
              .refinedWith(tpnme.MirroredElemLabels, TypeAlias(elemsLabels))
          val mirrorRef =
            if (cls.is(Scala2x)) anonymousMirror(monoType, ExtendsProductMirror, span)
            else companionPath(mirroredType, span)
          mirrorRef.cast(mirrorType)
        else EmptyTree
  end productMirror

  private def sumMirror(mirroredType: Type, formal: Type, span: Span)(using Context): Tree =
    val cls = mirroredType.classSymbol
    val useCompanion = cls.useCompanionAsMirror

    if cls.isGenericSum(if useCompanion then cls.linkedClass else ctx.owner) then
      val elemLabels = cls.children.map(c => ConstantType(Constant(c.name.toString)))

      def solve(sym: Symbol): Type = sym match
        case caseClass: ClassSymbol =>
          assert(caseClass.is(Case))
          if caseClass.is(Module) then
            caseClass.sourceModule.termRef
          else
            caseClass.primaryConstructor.info match
              case info: PolyType =>
                // Compute the the full child type by solving the subtype constraint
                // `C[X1, ..., Xn] <: P`, where
                //
                //   - P is the current `mirroredType`
                //   - C is the child class, with type parameters X1, ..., Xn
                //
                // Contravariant type parameters are minimized, all other type parameters are maximized.
                def instantiate(using Context) =
                  val poly = constrained(info, untpd.EmptyTree)._1
                  val resType = poly.finalResultType
                  val target = mirroredType match
                    case tp: HKTypeLambda => tp.resultType
                    case tp => tp
                  resType <:< target
                  val tparams = poly.paramRefs
                  val variances = caseClass.typeParams.map(_.paramVarianceSign)
                  val instanceTypes = tparams.lazyZip(variances).map((tparam, variance) =>
                    TypeComparer.instanceType(tparam, fromBelow = variance < 0))
                  resType.substParams(poly, instanceTypes)
                instantiate(using ctx.fresh.setExploreTyperState().setOwner(caseClass))
              case _ =>
                caseClass.typeRef
        case child => child.termRef
      end solve

      val (monoType, elemsType) = mirroredType match
        case mirroredType: HKTypeLambda =>
          val elems = mirroredType.derivedLambdaType(
            resType = TypeOps.nestedPairs(cls.children.map(solve))
          )
          (mkMirroredMonoType(mirroredType), elems)
        case _ =>
          val elems = TypeOps.nestedPairs(cls.children.map(solve))
          (mirroredType, elems)

      val mirrorType =
          mirrorCore(defn.Mirror_SumClass, monoType, mirroredType, cls.name, formal)
          .refinedWith(tpnme.MirroredElemTypes, TypeAlias(elemsType))
          .refinedWith(tpnme.MirroredElemLabels, TypeAlias(TypeOps.nestedPairs(elemLabels)))
      val mirrorRef =
        if useCompanion then companionPath(mirroredType, span)
        else anonymousMirror(monoType, ExtendsSumMirror, span)
      mirrorRef.cast(mirrorType)
    else EmptyTree
  end sumMirror

  def makeMirror
      (synth: (Type, Type, Span) => Context ?=> Tree, formal: Type, span: Span)
      (using Context): Tree =
    if checkFormal(formal) then
      formal.member(tpnme.MirroredType).info match
        case TypeBounds(mirroredType, _) => synth(mirroredType.stripTypeVar, formal, span)
        case other => EmptyTree
    else EmptyTree

  /** An implied instance for a type of the form `Mirror.Product { type MirroredType = T }`
   *  where `T` is a generic product type or a case object or an enum case.
   */
  val synthesizedProductMirror: SpecialHandler = (formal, span) =>
    makeMirror(productMirror, formal, span)

  /** An implied instance for a type of the form `Mirror.Sum { type MirroredType = T }`
   *  where `T` is a generic sum type.
   */
  val synthesizedSumMirror: SpecialHandler = (formal, span) =>
    makeMirror(sumMirror, formal, span)

  /** An implied instance for a type of the form `Mirror { type MirroredType = T }`
   *  where `T` is a generic sum or product or singleton type.
   */
  val synthesizedMirror: SpecialHandler = (formal, span) =>
    formal.member(tpnme.MirroredType).info match
      case TypeBounds(mirroredType, _) =>
        if mirroredType.termSymbol.is(CaseVal)
           || mirroredType.classSymbol.isGenericProduct
        then
          synthesizedProductMirror(formal, span)
        else
          synthesizedSumMirror(formal, span)
      case _ => EmptyTree

  val specialHandlers = List(
    defn.ClassTagClass        -> synthesizedClassTag,
    defn.TypeTestClass        -> synthesizedTypeTest,
    defn.CanEqualClass        -> synthesizedCanEqual,
    defn.ValueOfClass         -> synthesizedValueOf,
    defn.Mirror_ProductClass  -> synthesizedProductMirror,
    defn.Mirror_SumClass      -> synthesizedSumMirror,
    defn.MirrorClass          -> synthesizedMirror)

  def tryAll(formal: Type, span: Span)(using Context): Tree =
    def recur(handlers: SpecialHandlers): Tree = handlers match
      case (cls, handler) :: rest =>
        def baseWithRefinements(tp: Type): Type = tp.dealias match
          case tp @ RefinedType(parent, rname, rinfo) =>
            tp.derivedRefinedType(baseWithRefinements(parent), rname, rinfo)
          case _ =>
            tp.baseType(cls)
        val base = baseWithRefinements(formal)
        val result =
          if (base <:< formal.widenExpr)
            // With the subtype test we enforce that the searched type `formal` is of the right form
            handler(base, span)
          else EmptyTree
        result.orElse(recur(rest))
      case Nil =>
        EmptyTree
    recur(specialHandlers)

end Synthesizer
