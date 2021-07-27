package dotty.tools.dotc
package transform

import dotty.tools.dotc.ast.{Trees, tpd, untpd}
import scala.collection.mutable
import core._
import dotty.tools.dotc.typer.Checking
import dotty.tools.dotc.typer.Inliner
import dotty.tools.dotc.typer.VarianceChecker
import Types._, Contexts._, Names._, Flags._, DenotTransformers._, Phases._
import SymDenotations._, StdNames._, Annotations._, Trees._, Scopes._
import Decorators._
import Symbols._, SymUtils._, NameOps._
import ContextFunctionResults.annotateContextResults
import config.Printers.typr
import reporting._

object PostTyper {
  val name: String = "posttyper"
}

/** A macro transform that runs immediately after typer and that performs the following functions:
 *
 *  (1) Add super accessors (@see SuperAccessors)
 *
 *  (2) Convert parameter fields that have the same name as a corresponding
 *      public parameter field in a superclass to a forwarder to the superclass
 *      field (corresponding = super class field is initialized with subclass field)
 *      @see forwardParamAccessors.
 *
 *  (3) Add synthetic members (@see SyntheticMembers)
 *
 *  (4) Check that `New` nodes can be instantiated, and that annotations are valid
 *
 *  (5) Convert all trees representing types to TypeTrees.
 *
 *  (6) Check the bounds of AppliedTypeTrees
 *
 *  (7) Insert `.package` for selections of package object members
 *
 *  (8) Replaces self references by name with `this`
 *
 *  (9) Adds SourceFile annotations to all top-level classes and objects
 *
 *  (10) Adds Child annotations to all sealed classes
 *
 *  (11) Minimizes `call` fields of `Inlined` nodes to just point to the toplevel
 *       class from which code was inlined.
 *
 *  The reason for making this a macro transform is that some functions (in particular
 *  super and protected accessors and instantiation checks) are naturally top-down and
 *  don't lend themselves to the bottom-up approach of a mini phase. The other two functions
 *  (forwarding param accessors and synthetic methods) only apply to templates and fit
 *  mini-phase or subfunction of a macro phase equally well. But taken by themselves
 *  they do not warrant their own group of miniphases before pickling.
 */
class PostTyper extends MacroTransform with IdentityDenotTransformer { thisPhase =>
  import tpd._

  /** the following two members override abstract members in Transform */
  override def phaseName: String = PostTyper.name

  override def checkPostCondition(tree: tpd.Tree)(using Context): Unit = tree match {
    case tree: ValOrDefDef =>
      assert(!tree.symbol.signature.isUnderDefined)
    case _ =>
  }

  override def changesMembers: Boolean = true // the phase adds super accessors and synthetic members

  override def transformPhase(using Context): Phase = thisPhase.next

  def newTransformer(using Context): Transformer =
    new PostTyperTransformer

  val superAcc: SuperAccessors = new SuperAccessors(thisPhase)
  val synthMbr: SyntheticMembers = new SyntheticMembers(thisPhase)
  val beanProps: BeanProperties = new BeanProperties(thisPhase)

  private def newPart(tree: Tree): Option[New] = methPart(tree) match {
    case Select(nu: New, _) => Some(nu)
    case _ => None
  }

  private def checkValidJavaAnnotation(annot: Tree)(using Context): Unit = {
    // TODO fill in
  }

  class PostTyperTransformer extends Transformer {

    private var inJavaAnnot: Boolean = false

    private var noCheckNews: Set[New] = Set()

    def withNoCheckNews[T](ts: List[New])(op: => T): T = {
      val saved = noCheckNews
      noCheckNews ++= ts
      try op finally noCheckNews = saved
    }

    def isCheckable(t: New): Boolean = !inJavaAnnot && !noCheckNews.contains(t)

    /** Mark parameter accessors that are aliases of like-named parameters
     *  in their superclass with SuperParamAlias.
     *  This info is used in phase ParamForwarding
     */
    private def forwardParamAccessors(impl: Template)(using Context): Unit = impl.parents match
      case superCall @ Apply(fn, superArgs) :: _ if superArgs.nonEmpty =>
        fn.tpe.widen match
          case MethodType(superParamNames) =>
            for case stat: ValDef <- impl.body do
              val sym = stat.symbol
              if sym.isAllOf(PrivateParamAccessor, butNot = Mutable)
                 && !sym.info.isInstanceOf[ExprType] // val-parameters cannot be call-by name, so no need to try to forward to them
              then
                val idx = superArgs.indexWhere(_.symbol == sym)
                if idx >= 0 && superParamNames(idx) == stat.name then
                  // Supercall to like-named parameter.
                  // Having it have the same name is needed to maintain correctness in presence of subclassing
                  // if you would use parent param-name `a` to implement param-field `b`
                  // overriding field `b` will actually override field `a`, that is wrong!
                  typr.println(i"super alias: ${sym.showLocated}")
                  sym.setFlagFrom(thisPhase, SuperParamAlias)
          case _ =>
      case _ =>

    private def transformAnnot(annot: Tree)(using Context): Tree = {
      val saved = inJavaAnnot
      inJavaAnnot = annot.symbol.is(JavaDefined)
      if (inJavaAnnot) checkValidJavaAnnotation(annot)
      try transform(annot)
      finally inJavaAnnot = saved
    }

    private def transformAnnot(annot: Annotation)(using Context): Annotation =
      annot.derivedAnnotation(transformAnnot(annot.tree))

    private def processMemberDef(tree: Tree)(using Context): tree.type = {
      val sym = tree.symbol
      Checking.checkValidOperator(sym)
      sym.transformAnnotations(transformAnnot)
      sym.defTree = tree
      tree
    }

    private def processValOrDefDef(tree: Tree)(using Context): tree.type =
      val sym = tree.symbol
      tree match
        case tree: ValOrDefDef if !sym.is(Synthetic) =>
          checkInferredWellFormed(tree.tpt)
          if sym.is(Method) then
            if sym.isSetter then
              removeUnwantedAnnotations(sym, defn.SetterMetaAnnot, NoSymbol, keepIfNoRelevantAnnot = false)
          else
            if sym.is(Param) then
              removeUnwantedAnnotations(sym, defn.ParamMetaAnnot, NoSymbol, keepIfNoRelevantAnnot = true)
            else
              removeUnwantedAnnotations(sym, defn.GetterMetaAnnot, defn.FieldMetaAnnot, keepIfNoRelevantAnnot = !sym.is(ParamAccessor))
          if sym.isScala2Macro && !ctx.settings.XignoreScala2Macros.value then
            if !sym.owner.unforcedDecls.exists(p => !p.isScala2Macro && p.name == sym.name && p.signature == sym.signature)
               // Allow scala.reflect.materializeClassTag to be able to compile scala/reflect/package.scala
               // This should be removed on Scala 3.x
               && sym.owner != defn.ReflectPackageClass
            then
              report.error("No Scala 3 implementation found for this Scala 2 macro.", tree.srcPos)
        case _ =>
      processMemberDef(tree)

    private def checkInferredWellFormed(tree: Tree)(using Context): Unit = tree match
      case tree: TypeTree
      if tree.span.isZeroExtent
          // don't check TypeTrees with non-zero extent;
          // these are derived from explicit types
         && !ctx.reporter.errorsReported
          // don't check if errors were already reported; this avoids follow-on errors
          // for inferred types if explicit types are already ill-formed
        => Checking.checkAppliedTypesIn(tree)
      case _ =>

    private def removeUnwantedAnnotations(sym: Symbol, metaAnnotSym: Symbol,
        metaAnnotSymBackup: Symbol, keepIfNoRelevantAnnot: Boolean)(using Context): Unit =
      def shouldKeep(annot: Annotation): Boolean =
        val annotSym = annot.symbol
        annotSym.hasAnnotation(metaAnnotSym)
          || annotSym.hasAnnotation(metaAnnotSymBackup)
          || (keepIfNoRelevantAnnot && {
            !annotSym.annotations.exists(metaAnnot => defn.FieldAccessorMetaAnnots.contains(metaAnnot.symbol))
          })
      if sym.annotations.nonEmpty then
        sym.filterAnnotations(shouldKeep(_))

    private def transformSelect(tree: Select, targs: List[Tree])(using Context): Tree = {
      val qual = tree.qualifier
      qual.symbol.moduleClass.denot match {
        case pkg: PackageClassDenotation =>
          val pobj = pkg.packageObjFor(tree.symbol)
          if (pobj.exists)
            return transformSelect(cpy.Select(tree)(qual.select(pobj).withSpan(qual.span), tree.name), targs)
        case _ =>
      }
      val tree1 = super.transform(tree)
      constToLiteral(tree1) match {
        case _: Literal => tree1
        case _ => superAcc.transformSelect(tree1, targs)
      }
    }

    private def normalizeTypeArgs(tree: TypeApply)(using Context): TypeApply = tree.tpe match {
      case pt: PolyType => // wait for more arguments coming
        tree
      case _ =>
        def decompose(tree: TypeApply): (Tree, List[Tree]) = tree.fun match {
          case fun: TypeApply =>
            val (tycon, args) = decompose(fun)
            (tycon, args ++ tree.args)
          case _ =>
            (tree.fun, tree.args)
        }
        def reorderArgs(pnames: List[Name], namedArgs: List[NamedArg], otherArgs: List[Tree]): List[Tree] = pnames match {
          case pname :: pnames1 =>
            namedArgs.partition(_.name == pname) match {
              case (NamedArg(_, arg) :: _, namedArgs1) =>
                arg :: reorderArgs(pnames1, namedArgs1, otherArgs)
              case _ =>
                val otherArg :: otherArgs1 = otherArgs
                otherArg :: reorderArgs(pnames1, namedArgs, otherArgs1)
            }
          case nil =>
            assert(namedArgs.isEmpty && otherArgs.isEmpty)
            Nil
        }
        val (tycon, args) = decompose(tree)
        tycon.tpe.widen match {
          case tp: PolyType if args.exists(isNamedArg) =>
            val (namedArgs, otherArgs) = args.partition(isNamedArg)
            val args1 = reorderArgs(tp.paramNames, namedArgs.asInstanceOf[List[NamedArg]], otherArgs)
            TypeApply(tycon, args1).withSpan(tree.span).withType(tree.tpe)
          case _ =>
            tree
        }
    }

    private object dropInlines extends TreeMap {
      override def transform(tree: Tree)(using Context): Tree = tree match {
        case Inlined(call, _, expansion) =>
          val newExpansion = PruneErasedDefs.trivialErasedTree(tree)
          cpy.Inlined(tree)(call, Nil, newExpansion)
        case _ => super.transform(tree)
      }
    }

    def checkNoConstructorProxy(tree: Tree)(using Context): Unit =
      if tree.symbol.is(ConstructorProxy) then
        report.error(em"constructor proxy ${tree.symbol} cannot be used as a value", tree.srcPos)

    override def transform(tree: Tree)(using Context): Tree =
      try tree match {
        // TODO move CaseDef case lower: keep most probable trees first for performance
        case CaseDef(pat, _, _) =>
          val gadtCtx =
           pat.removeAttachment(typer.Typer.InferredGadtConstraints) match
             case Some(gadt) => ctx.fresh.setGadt(gadt)
             case None =>
               ctx
          super.transform(tree)(using gadtCtx)
        case tree: Ident if !tree.isType =>
          if tree.symbol.is(Inline) && !Inliner.inInlineMethod then
            ctx.compilationUnit.needsInlining = true
          checkNoConstructorProxy(tree)
          tree.tpe match {
            case tpe: ThisType => This(tpe.cls).withSpan(tree.span)
            case _ => tree
          }
        case tree @ Select(qual, name) =>
          if tree.symbol.is(Inline) then
            ctx.compilationUnit.needsInlining = true
          if (name.isTypeName) {
            Checking.checkRealizable(qual.tpe, qual.srcPos)
            withMode(Mode.Type)(super.transform(tree))
          }
          else
            checkNoConstructorProxy(tree)
            transformSelect(tree, Nil)
        case tree: Apply =>
          val methType = tree.fun.tpe.widen
          val app =
            if (methType.isErasedMethod)
              tpd.cpy.Apply(tree)(
                tree.fun,
                tree.args.mapConserve(arg =>
                  if (methType.isImplicitMethod && arg.span.isSynthetic)
                    PruneErasedDefs.trivialErasedTree(arg)
                  else dropInlines.transform(arg)))
            else
              tree
          def app1 =
   		    	// reverse order of transforming args and fun. This way, we get a chance to see other
   			    // well-formedness errors before reporting errors in possible inferred type args of fun.
            val args1 = transform(app.args)
            cpy.Apply(app)(transform(app.fun), args1)
          methPart(app) match
            case Select(nu: New, nme.CONSTRUCTOR) if isCheckable(nu) =>
              // need to check instantiability here, because the type of the New itself
              // might be a type constructor.
              ctx.typer.checkClassType(tree.tpe, tree.srcPos, traitReq = false, stablePrefixReq = true)
              if !nu.tpe.isLambdaSub then
                // Check the constructor type as well; it could be an illegal singleton type
                // which would not be reflected as `tree.tpe`
                ctx.typer.checkClassType(nu.tpe, tree.srcPos, traitReq = false, stablePrefixReq = false)
              Checking.checkInstantiable(tree.tpe, nu.srcPos)
              withNoCheckNews(nu :: Nil)(app1)
            case _ =>
              app1
        case UnApply(fun, implicits, patterns) =>
          // Reverse transform order for the same reason as in `app1` above.
          val patterns1 = transform(patterns)
          cpy.UnApply(tree)(transform(fun), transform(implicits), patterns1)
        case tree: TypeApply =>
          if tree.symbol.isQuote then
            ctx.compilationUnit.needsStaging = true
            ctx.compilationUnit.needsQuotePickling = true
          if tree.symbol.is(Inline) then
            ctx.compilationUnit.needsInlining = true
          val tree1 @ TypeApply(fn, args) = normalizeTypeArgs(tree)
          args.foreach(checkInferredWellFormed)
          if (fn.symbol != defn.ChildAnnot.primaryConstructor)
            // Make an exception for ChildAnnot, which should really have AnyKind bounds
            Checking.checkBounds(args, fn.tpe.widen.asInstanceOf[PolyType])
          fn match {
            case sel: Select =>
              val args1 = transform(args)
              val sel1 = transformSelect(sel, args1)
              cpy.TypeApply(tree1)(sel1, args1)
            case _ =>
              super.transform(tree1)
          }
        case Inlined(call, bindings, expansion) if !call.isEmpty =>
          val pos = call.sourcePos
          val callTrace = Inliner.inlineCallTrace(call.symbol, pos)(using ctx.withSource(pos.source))
          cpy.Inlined(tree)(callTrace, transformSub(bindings), transform(expansion)(using inlineContext(call)))
        case templ: Template =>
          withNoCheckNews(templ.parents.flatMap(newPart)) {
            forwardParamAccessors(templ)
            synthMbr.addSyntheticMembers(
              beanProps.addBeanMethods(
                superAcc.wrapTemplate(templ)(
                  super.transform(_).asInstanceOf[Template]))
            )
          }
        case tree: ValDef =>
          val tree1 = cpy.ValDef(tree)(rhs = normalizeErasedRhs(tree.rhs, tree.symbol))
          processValOrDefDef(super.transform(tree1))
        case tree: DefDef =>
          annotateContextResults(tree)
          val tree1 = cpy.DefDef(tree)(rhs = normalizeErasedRhs(tree.rhs, tree.symbol))
          processValOrDefDef(superAcc.wrapDefDef(tree1)(super.transform(tree1).asInstanceOf[DefDef]))
        case tree: TypeDef =>
          val sym = tree.symbol
          if (sym.isClass)
            VarianceChecker.check(tree)
            annotateExperimental(sym)
            // Add SourceFile annotation to top-level classes
            if sym.owner.is(Package)
               && ctx.compilationUnit.source.exists
               && sym != defn.SourceFileAnnot
            then
              val reference = ctx.settings.sourceroot.value
              val relativePath = util.SourceFile.relativePath(ctx.compilationUnit.source, reference)
              sym.addAnnotation(Annotation.makeSourceFile(relativePath))
          else (tree.rhs, sym.info) match
            case (rhs: LambdaTypeTree, bounds: TypeBounds) =>
              VarianceChecker.checkLambda(rhs, bounds)
            case _ =>
          processMemberDef(super.transform(tree))
        case tree: New if isCheckable(tree) =>
          Checking.checkInstantiable(tree.tpe, tree.srcPos)
          super.transform(tree)
        case tree: Closure if !tree.tpt.isEmpty =>
          Checking.checkRealizable(tree.tpt.tpe, tree.srcPos, "SAM type")
          super.transform(tree)
        case tree @ Annotated(annotated, annot) =>
          cpy.Annotated(tree)(transform(annotated), transformAnnot(annot))
        case tree: AppliedTypeTree =>
          if (tree.tpt.symbol == defn.andType)
            Checking.checkNonCyclicInherited(tree.tpe, tree.args.tpes, EmptyScope, tree.srcPos)
              // Ideally, this should be done by Typer, but we run into cyclic references
              // when trying to typecheck self types which are intersections.
          else if (tree.tpt.symbol == defn.orType)
            () // nothing to do
          else
            Checking.checkAppliedType(tree)
          super.transform(tree)
        case SingletonTypeTree(ref) =>
          Checking.checkRealizable(ref.tpe, ref.srcPos)
          super.transform(tree)
        case tree: TypeTree =>
          tree.withType(
            tree.tpe match {
              case AnnotatedType(tpe, annot) => AnnotatedType(tpe, transformAnnot(annot))
              case tpe => tpe
            }
          )
        case Import(expr, selectors) =>
          val exprTpe = expr.tpe
          val seen = mutable.Set.empty[Name]

          def checkIdent(sel: untpd.ImportSelector): Unit =
            if !exprTpe.member(sel.name).exists
               && !exprTpe.member(sel.name.toTypeName).exists then
              report.error(NotAMember(exprTpe, sel.name, "value"), sel.imported.srcPos)
            if seen.contains(sel.name) then
              report.error(ImportRenamedTwice(sel.imported), sel.imported.srcPos)
            seen += sel.name

          for sel <- selectors do
            if !sel.isWildcard then checkIdent(sel)
          super.transform(tree)
        case Typed(Ident(nme.WILDCARD), _) =>
          withMode(Mode.Pattern)(super.transform(tree))
            // The added mode signals that bounds in a pattern need not
            // conform to selector bounds. I.e. assume
            //     type Tree[T >: Null <: Type]
            // One is still allowed to write
            //     case x: Tree[?]
            // (which translates to)
            //     case x: (_: Tree[?])
        case m @ MatchTypeTree(bounds, selector, cases) =>
          // Analog to the case above for match types
          def transformIgnoringBoundsCheck(x: CaseDef): CaseDef =
            withMode(Mode.Pattern)(super.transform(x)).asInstanceOf[CaseDef]
          cpy.MatchTypeTree(tree)(
            super.transform(bounds),
            super.transform(selector),
            cases.mapConserve(transformIgnoringBoundsCheck)
          )
        case Block(_, Closure(_, _, tpt)) if ExpandSAMs.needsWrapperClass(tpt.tpe) =>
          superAcc.withInvalidCurrentClass(super.transform(tree))
        case tree =>
          super.transform(tree)
      }
      catch {
        case ex : AssertionError =>
          println(i"error while transforming $tree")
          throw ex
      }

    /** Transforms the rhs tree into a its default tree if it is in an `erased` val/def.
     *  Performed to shrink the tree that is known to be erased later.
     */
    private def normalizeErasedRhs(rhs: Tree, sym: Symbol)(using Context) =
      if (sym.isEffectivelyErased) dropInlines.transform(rhs) else rhs

    private def annotateExperimental(sym: Symbol)(using Context): Unit =
      if sym.is(Module) && sym.companionClass.hasAnnotation(defn.ExperimentalAnnot) then
        sym.addAnnotation(defn.ExperimentalAnnot)
        sym.companionModule.addAnnotation(defn.ExperimentalAnnot)

  }
}
