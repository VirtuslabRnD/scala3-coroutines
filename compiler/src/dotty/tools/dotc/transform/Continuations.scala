package dotty.tools.dotc
package transform

import scala.collection.mutable

import core.*
import Constants.Constant
import Contexts.*
import Decorators.*
import DenotTransformers.*
import Flags.*
import MegaPhase.*
import ast.tpd.*
import Symbols.*
import StdNames.*
import Types.*
import typer.Inliner
import Names.Name
import dotty.tools.dotc.report
import dotty.tools.dotc.util.Property
import dotty.tools.dotc.ast.Trees

object Continuations:
  private val CoroutineDefInfo = Property.StickyKey[(DefDef, Type)]

  class CoroutineStubs extends MiniPhase:
    def phaseName = "coroutineStubs"

    override def transformDefDef(tree: DefDef)(using Context) =
      val orig = tree.symbol.asTerm
      val coroutine = Option.unless(orig.is(Inline) || orig.isAnonymousFunction)(orig.info).flatMap(coroutineInfo).map { cInfo =>
        val sym = orig.copy(
          name = orig.name ++ "$coroutine",
          info = cInfo.coroutineType,
          flags = orig.flags | Synthetic
        ).asTerm.entered

        DefDef(sym, Literal(Constant(null))).withAttachment(CoroutineDefInfo, (tree, cInfo.returnType))
      }

      coroutine match
        case Some(c) => Thicket(c, tree)
        case _ => tree
  end CoroutineStubs
  class Transform extends MiniPhase with IdentityDenotTransformer:
    thisPhase =>
    def phaseName = "continuations"

    override def runsAfter = Set("coroutineStubs", PatternMatcher.name)

    override def transformDefDef(tree: DefDef)(using Context) = tree.getAttachment(CoroutineDefInfo) match
      case Some((origDef, result)) =>
        val origSym = origDef.symbol.asTerm
        def rhs(paramss: List[List[Symbol]]) =
          val mapping: Map[Symbol, Tree] = origSym.paramSymss.zip(paramss).flatMap(_ zip _).map((a, b) => (a, ref(b))).toMap
          val stabilizer = TermRef(NoPrefix, paramss.last.last)
          procClosure(stabilizer, result, mapping)(origDef.rhs)
        val res = cpy.DefDef(tree)(rhs = rhs(tree.symbol.paramSymss))
        reown(res.rhs, tree.symbol) // TODO: this is overkill
        res
      case None => tree

    override def transformApply(tree: Apply)(using Context): Tree = tree.fun match
      case TypeApply(Select(a, f), t::Nil) if tree.symbol.exists && tree.symbol.owner == defn.CoroutineExecutor && tree.symbol.name == nme.run =>
        val answerType = t.tpe
        val stabilizer = newSymbol(ctx.owner, "$stabilizer".toTermName, Synthetic, a.tpe).entered
        val stabilizerVal = ValDef(stabilizer.asTerm, a)
        Block(stabilizerVal :: Nil, cpy.Apply(tree)(
          ref(stabilizer).select("process".toTermName).appliedToType(answerType),
          tree.args.map(procClosure(TermRef(NoPrefix, stabilizer), answerType, Map.empty))
        ))
      case _ => tree

    private def procClosure(typePrefix: NamedType, answerType: Type, params: Map[Symbol, Tree])(using Context): Tree => Tree =
      case b @ Block((d: DefDef) :: Nil, _) =>
        val coroutine = generateCoroutine(d.rhs, d.symbol.owner, typePrefix, answerType, params)
        Block(coroutine :: Nil, New(coroutine.symbol.asType.typeRef, Nil))
      case t => // TODO filter out things that do not need to be wrapped in a coroutine
        val coroutine = generateCoroutine(t, NoSymbol, typePrefix, answerType, params)
        Block(coroutine :: Nil, New(coroutine.symbol.asType.typeRef, Nil))

    private def generateCoroutine(rhs: Tree, owner: Symbol, typePrefix: NamedType, answerType: Type, params: Map[Symbol, Tree])(using Context): TypeDef =
      val tpe = TypeRef(typePrefix, "Coroutine".toTypeName).appliedTo(answerType)
      val cls = newNormalizedClassSymbol(owner, tpnme.ANON_CLASS, Synthetic | Final, tpe :: Nil)
      val constr = DefDef(newConstructor(cls, Synthetic, Nil, Nil).entered)

      val stackChT = TypeRef(typePrefix, "StackChange".toTypeName)
      val extractT = TypeRef(typePrefix, "Extract".toTypeName)
      def makeId(id: Int): Tree = Literal(Constant(id))


      val gotoResultTpe = OrType.make(stackChT, defn.IntType, false)

      val gotoSym = newSymbol(
        owner = cls,
        name = "goto".toTermName,
        flags = Method | Private | Synthetic,
        info = MethodType(("stateId" :: "t" :: Nil).map(_.toTermName), defn.IntType :: extractT :: Nil, gotoResultTpe)
      ).asTerm.entered

      given lifter: LiftState = LiftState(typePrefix, cls, params, gotoSym)
      val analysis = analyze(answerType)(rhs).simplifyOrWrap(lifter.markStackPop)
      val vars = lifter.liftedTrees

      def gotoRhs(argss: List[List[Tree]]): Tree =
        val (idRef :: extractRef :: Nil) :: Nil = argss: @unchecked
        val entry = CaseDef(makeId(0), EmptyTree, analysis.head.toTree)
        val states = analysis.nodes.map { node => CaseDef(makeId(node.id), EmptyTree, (node.init(extractRef) :: node.rest).toTree) }
        val fallback = Throw(New(defn.IllegalArgumentExceptionType, Nil))
        Match(idRef, entry +: states :+ CaseDef(Underscore(idRef.tpe), EmptyTree, fallback))
      end gotoRhs

      val gotoTree = DefDef(gotoSym, gotoRhs)
      reown(gotoTree.rhs, gotoSym)


      val dispatchSym = newSymbol(
        owner = cls,
        name = "dispatch".toTermName,
        flags = Method | Private | Synthetic,
        info = MethodType(("stateId" :: "t" :: Nil).map(_.toTermName), defn.IntType :: extractT :: Nil, stackChT)
      ).asTerm.entered

      def dispatchRhs(argss: List[List[Tree]]): Tree =
        val (idRef :: extractRef :: Nil) :: Nil = argss: @unchecked
        val stateOrIdSym = newSymbol(
          owner = dispatchSym,
          name = "stateOrId".toTermName,
          flags = Synthetic,
          info = OrType(stackChT, defn.IntType, soft = false)
        )
        val stateOrIdRhs = ref(gotoSym).appliedTo(idRef, extractRef)
        val resultValDef = ValDef(stateOrIdSym, stateOrIdRhs)
        val cond = ref(stateOrIdSym).select(defn.Any_isInstanceOf).appliedToType(stackChT)
        val ifTrue = ref(stateOrIdSym).select(defn.Any_asInstanceOf).appliedToType(stackChT)
        val ifFalse = ref(dispatchSym).appliedTo(
          ref(stateOrIdSym).select(defn.Any_asInstanceOf).appliedToType(defn.IntType),
          Literal(Constant(null)).asInstance(extractT)
        )
        val ifExpr = If(cond, ifTrue, ifFalse)
        Block(List(resultValDef), ifExpr)
      end dispatchRhs

      val dispatchTree = DefDef(dispatchSym, dispatchRhs)


      val startSym = defn.Coroutine_start.copy(
        owner = cls,
        info = MethodType(Nil, Nil, stackChT),
        flags = Synthetic | Method | Final | Override | Protected
      ).asTerm.entered

      val startRhs = ref(dispatchSym).appliedTo(Literal(Constant(0)), Literal(Constant(null)).asInstance(extractT))
      val startTree = DefDef(startSym, startRhs)

      val resumeSym = defn.Coroutine_resume.copy(
        owner = cls,
        info = MethodType(("stateId" :: "t" :: Nil).map(_.toTermName), defn.IntType :: extractT :: Nil, stackChT),
        flags = Synthetic | Method | Final | Override | Protected
      ).asTerm.entered
      def resumeRhs(argss: List[List[Tree]]): Tree =
        val (idRef :: extractRef :: Nil) :: Nil = argss: @unchecked
        ref(dispatchSym).appliedTo(idRef, extractRef)
      val resumeTree = DefDef(resumeSym, resumeRhs)

      val supertype = New(tpe, Nil)

      ClassDefWithParents(cls, constr, supertype::Nil, gotoTree :: dispatchTree :: startTree :: resumeTree :: vars)
    end generateCoroutine

    private def reown(block: Tree, newOwner: Symbol)(using Context) =
      object Reowner extends TreeTraverser:
        override def traverse(tree: Tree)(using Context) = tree match
          case m: MemberDef => m.symbol.copySymDenotation(owner = newOwner).installAfter(thisPhase)
          case _ => traverseChildren(tree)
      Reowner.traverse(block)

  /* === analysis === */

  private object ApplyCoroutine:
    def unapply(tree: Tree)(using Context): Option[(Tree, List[Tree])] =
      tree match
        case Apply(Select(Apply(call, args), _), _ :: Nil) if isCoroutineLike(call.symbol.info) => Some(call, args)
        case Apply(Apply(call, args), _ :: Nil) if isCoroutineLike(call.symbol.info) => Some(call, args)
        case _ => None

  private class LiftState(typePrefix: NamedType, cls: ClassSymbol, params: Map[Symbol, Tree], val gotoSym: Symbol)(using Context):
    private val stackChMod = ref(typePrefix).select("StackChange".toTermName)
    private val stackPop = stackChMod.select("Pop".toTermName).select(nme.apply)
    private val stackPush = stackChMod.select("Push".toTermName).select(nme.apply)
    private val progress = stackChMod.select("Progress".toTermName).select(nme.apply)

    val extractT = TypeRef(typePrefix, "Extract".toTypeName)

    private var states: Int = 0
    private var counter: Int = 0
    private var rhses: Map[Symbol, Tree] = Map.empty
    private var symbols: List[TermSymbol] = Nil
    private var statesOfLabels = Map.empty[Name, (Int, Symbol)]
    private val lifted = mutable.Map[Symbol, Symbol]()

    def makeMethod(original: Symbol, rhs: Tree)(using Context) =
      val tpe = original.info match
        case m: MethodType => m
        case ExprType(result) => MethodType(Nil, Nil, result)
        case _ =>
          report.error("Unsupported method type in the coroutine", original.defTree.srcPos)
          NoType
      val sym = createSymbol(Synthetic | Private | Method, tpe, original)
      addRhs(sym, rhs)

    def make(tpe: Type, original: Symbol = NoSymbol)(using Context): TermSymbol =
      val symTpe = tpe match
        case MethodTpe(_, args, res) => defn.FunctionOf(args, res)
        case t => t
      createSymbol(Synthetic | Private | Mutable, symTpe, original)

    private def createSymbol(flags: FlagSet, tpe: Type, original: Symbol)(using Context): TermSymbol =
      val suffix = if original == NoSymbol then "" else "_$" + original.name.toString
      val sym = newSymbol(
        owner = cls,
        name = ("$lift_" + counter + suffix).toTermName,
        flags = flags,
        info = tpe
      ).entered.asTerm
      counter += 1
      symbols = sym :: symbols

      if original != NoSymbol then lifted += (original -> sym)
      sym

    def addRhs(sym: Symbol, rhs: Tree) = rhses = rhses.updated(sym, rhs)

    def translateRef(tree: Tree): Tree =
      def local(sym: Symbol): Option[Tree] = lifted.get(sym).map { s => sym.info match
        case ExprType(_) => ref(s).appliedToNone
        case _ => ref(s)
      }
      params.get(tree.symbol) orElse local(tree.symbol) getOrElse tree

    def newState =
      states += 1
      states

    def hasLabel(name: Name): Boolean = statesOfLabels.isDefinedAt(name)

    def labelStateWithSym(name: Name): (Int, Symbol) = statesOfLabels(name)

    def rememberLabel(name: Name, state: Int, sym: Symbol): Unit =
      statesOfLabels += name -> (state, sym)

    def markProgress(to: Int)(tree: Tree): Tree =
      progress.appliedTo(tree, Literal(Constant(to)))

    def markStackPop(tree: Tree): Tree = stackPop.appliedTo(tree)

    def markStackPush(to: Int)(tree: Tree): Tree =
      stackPush.appliedTo(tree, Literal(Constant(to)))

    def executrorRef(using Context) = ref(typePrefix)

    def liftedTrees: List[Tree] = symbols.map { s => rhses.get(s) match
      case Some(rhs) => DefDef(s, rhs)
      case _ => ValDef(s)
    }
  end LiftState

  private def lifter(using l: LiftState) = l

  private def analyze(tpe: Type)(tree: Tree)(using LiftState, Context): PreCoroutine = tree match
    // TODO: for now we do not support suspending in nested functions so it should be sufficient
    case Block((_: DefDef) :: Nil, Closure(Nil, _, _)) => tree

    // blocks:
    case Block(stats, expr) =>
      val statsp = stats.map(analyze(NoType))
      val exprp = analyze(tpe)(expr)
      (statsp :+ exprp).reduce(_ combine _).simplify(tt => cpy.Block(tree)(tt.init, tt.last))
    case i: Inlined => analyze(tpe)(Inliner.dropInlined(i))

    // simple wrappable:
    case Typed(expr, tpt) => analyze(tpt.tpe)(expr).simplifyOrWrap(cpy.Typed(tree)(_, tpt))
    case Select(qual, name) => analyze(NoType)(qual).simplifyOrWrap(cpy.Select(tree)(_, name))
    case TypeApply(fn, args) => analyze(NoType)(fn).simplifyOrWrap(cpy.TypeApply(tree)(_, args))

    // always lifted:
    case v: ValDef =>
      val sym = lifter.make(v.symbol.info, v.symbol)
      analyze(sym.info)(v.rhs).simplifyOrWrap(t => cpy.Assign(v)(lhs = ref(sym), rhs = t))

    case d: DefDef =>
      analyze(d.symbol.info.resultType)(d.rhs) match
        case t: Trees => lifter.makeMethod(d.symbol, t.toTree)
        case _ => report.error("Suspending in nested functions is not yet supported", d.srcPos)
      PreCoroutine.empty

    // suspend - create the new coroutine:
    case Apply(call, arg :: Nil) if call.symbol.owner == defn.ContinuationContext && call.symbol.name.toString == "suspend" =>
      val newState = lifter.newState
      val left = analyze(lifter.extractT)(arg).simplifyOrWrap(lifter.markProgress(newState))
      left combine Coroutine.suspension(tree.tpe, newState)

    // call other coroutine
    case ApplyCoroutine(call, args) =>
      val argsLifts = (call.symbol.info.paramInfoss.head zip args).map(analyzeWithLift(_)(_))
      val newCall = ref(call.symbol.owner.info.member(call.symbol.name ++ "$coroutine").symbol) // TODO: this doesn't seem to be super reliable
      val newState = lifter.newState
      val combined = argsLifts.allLifted combine cpy.Apply(tree)(newCall.appliedToArgs(argsLifts.map(_.reference)), lifter.executrorRef :: Nil)
      combined.simplifyOrWrap(lifter.markStackPush(newState)) combine Coroutine.suspension(tree.tpe, newState)

    // trees that can suspend in different subtrees:
    case Apply(call, args) =>
      val callLift = analyzeWithLift(call.tpe.widen)(call)
      val argLifts = (call.tpe.widenDealias.paramInfoss.head zip args).map(analyzeWithLift(_)(_))
      if argLifts.forall(_.isSimple) then
        callLift.unlifted.simplifyOrWrap(c => cpy.Apply(tree)(c, argLifts.references))
      else
        callLift.lifted combine argLifts.allLifted combine cpy.Apply(tree)(callLift.reference, argLifts.references)

    case SeqLiteral(elems, tpt) =>
      val lifts = elems.map(analyzeWithLift(tpt.tpe))
      lifts.allLifted combine cpy.SeqLiteral(tree)(lifts.references, tpt)

    // trees that can branch and needs more complex logic:
    case If(cond, thenp, elsep) =>
      val condpc = analyze(defn.BooleanType)(cond)
      val commonTpe = tpe orElse TypeComparer.lub(thenp.tpe, elsep.tpe).widenIfUnstable
      (analyze(commonTpe)(thenp), analyze(commonTpe)(elsep)) match
        case (thent: Trees, elset: Trees) => condpc.simplifyOrWrap(i => cpy.If(tree)(i, thent.toTree, elset.toTree))
        case (thenpc, elsepc) =>
          val newState = lifter.newState
          val sym = lifter.make(commonTpe)
          def lift(pc: PreCoroutine) = pc.simplifyOrWrap(Assign(ref(sym), _)) combine Literal(Constant(newState))
          val thenl = lift(thenpc)
          val elsel = lift(elsepc)
          condpc.simplifyOrWrap(i => cpy.If(tree)(i, thenl.head.toTree, elsel.head.toTree)) combine
            thenl.headless combine
            elsel.headless combine
            Coroutine(Nil, Node(_ => ref(sym), Nil, commonTpe, newState) :: Nil)

    case Labeled(bind, expr) =>
      val newState = lifter.newState
      val sym = lifter.make(tpe)
      lifter.rememberLabel(bind.name, newState, sym)
      analyze(tpe)(expr) combine Coroutine(Nil, Node(_ => ref(sym), Nil, tree.tpe, newState) :: Nil)

    // We don't jump out of a state of a coroutine unless we jump out of the coroutine altogether
    case Return(expr, from) =>
      from match
        case Ident(labelName) if lifter.hasLabel(labelName) =>
          val (labelReturnStateId, labelReturnValSym) = lifter.labelStateWithSym(labelName)
          val body = analyze(labelReturnValSym.info)(expr).simplifyOrWrap(Assign(ref(labelReturnValSym), _))
          val ret = cpy.Return(tree)(Literal(Constant(labelReturnStateId)), ref(lifter.gotoSym))
          body combine ret
        case _ => analyze(NoType)(expr).simplifyOrWrap(cpy.Return(tree)(_, from)) // TODO handle subcases?

    case Closure(env, meth, tpt) =>
      val envLifts = env.map(analyzeWithLift(NoType))
      envLifts.allLifted combine analyze(NoType)(meth).simplifyOrWrap(cpy.Closure(tree)(envLifts.references, _, tpt))

    // may be local reference that was lifted
    case t: Ident =>
      lifter.translateRef(t)

    // do not need transformations:
    case _: New => tree
    case _: Literal  => tree
    case EmptyTree => tree

    // debug case TODO: remove
    case _ =>
      println("\n\n\t====")
      println(tree.show)
      println(tree)
      // System.exit(-1)
      tree


  private class Lift(val unlifted: PreCoroutine, tpe: Type)(using LiftState, Context):
    def isSimple = unlifted.isInstanceOf[Trees]

    lazy val (lifted: PreCoroutine, reference: Tree) = unlifted match
      case t: Tree => PreCoroutine.empty -> t
      case tt: List[Tree] => PreCoroutine.empty -> tt.toTree
      case Coroutine(head, nodes :+ last) =>
        val sym = lifter.make(tpe orElse last.tpe)
        val wrapped = tpe match
          case m: MethodType => last.wrap(t => Assign(ref(sym), Lambda(m, args => t.appliedToArgs(args))))
          case _ => last.wrap(Assign(ref(sym), _))
        Coroutine(head, nodes :+ wrapped) -> (if tpe.isInstanceOf[MethodType] then ref(sym).select(nme.apply) else ref(sym))
      case Coroutine(_, _) => throw AssertionError("Empty coroutine should never be constructed")

  extension (ll: List[Lift])
    private def references: List[Tree] = ll.map(_.reference)
    private def allLifted: PreCoroutine = ll.map(_.lifted).fold(PreCoroutine.empty)(_ combine _)

  private def analyzeWithLift(tpe: Type)(tree: Tree)(using LiftState, Context): Lift = Lift(analyze(tpe)(tree), tpe)


  /* === Context extraction === */
  case class CInfo(coroutineType: Type, returnType: Type)

  def isCoroutineLike(tpe: Type)(using Context): Boolean = coroutineInfo(tpe).nonEmpty

  // TODO: this could potentially be simplified but there are quite a lot of cases we don't handle yet and we shouldn't allow them to go unnoticed
  // TODO: Handle (contextual) function returning a contextual function (taking coroutine context as parameter)
  // Look at CoroutineInfo test for details what should fail
  private def coroutineInfo(tpe: Type)(using Context): Option[CInfo] = tpe match
    case mt @ MethodTpe(paramNames, paramTypes, returnType) =>
      val resultCoroutineInfo = coroutineInfo(returnType)
      if mt.isContextualMethod && paramTypes.exists(_.classSymbol == defn.ContinuationContext) then
        paramTypes match
          case TypeRef(exec, _) :: Nil =>
            if returnType.isInstanceOf[MethodOrPoly] then
              report.error("Implementation restriction: Coroutine context cannot be method's non-last contextual parameter")
              None
            else
              resultCoroutineInfo match
                case Some(_) =>
                  report.error("Implementation restriction: Coroutine contexts cannot be mixed")
                  None
                case None =>
                  val coroutineType =
                    MethodType("$executor".toTermName :: Nil)(_ => exec::Nil, m => m.newParamRef(0).select("Coroutine".toTypeName).appliedTo(returnType))
                  Some(CInfo(coroutineType, returnType))
          case _ =>
            report.error("Implementation restriction: Coroutine context has to be in its own parameters list")
            None
      else
        resultCoroutineInfo.map { cInfo =>
          val coroutineType = mt.newLikeThis(paramNames, paramTypes, cInfo.coroutineType)
          cInfo.copy(coroutineType = coroutineType)
        }
    case pt: PolyType =>
      coroutineInfo(pt.resType).map { cInfo =>
        val coroutineType = pt.newLikeThis(pt.paramNames, pt.paramInfos, cInfo.coroutineType)
        cInfo.copy(coroutineType = coroutineType)
      }
    case defn.ContextFunctionType(paramTypes, returnType, _) =>
      if paramTypes.exists(_.classSymbol == defn.ContinuationContext) then
        paramTypes match
          case TypeRef(exec, _) :: Nil =>
            val coroutineType =
              MethodType("$executor".toTermName :: Nil)(_ => exec::Nil, m => m.newParamRef(0).select("Coroutine".toTypeName).appliedTo(returnType))
            Some(CInfo(coroutineType, returnType))
          case _ =>
            report.error("Implementation restriction: Coroutine context has to be in its own parameters list")
            None
      else
        None
    case _ =>
      None

  /* === coroutines building blocks === */
  private case class Node(init: Tree => Tree, rest: List[Tree], tpe: Type, id: Int)(/* TODO: REMOVE THIS HACK */ using Context) extends printing.Showable:
    def append(trees: List[Tree]) = if trees.nonEmpty then copy(rest = rest ++ trees, tpe = trees.last.tpe) else this

    def wrap(f: Tree => Tree)(using Context): Node =
      if rest.isEmpty then copy(init = r => f(init(r)))
      else copy(rest = f(Block(rest.init, rest.last)) :: Nil)

    import printing.*, Texts._
    def toText(printer: Printer): Text =
      val sym = newSymbol(ctx.owner, "<|PLACEHOLDER|>".toTermName, Synthetic, NoType)
      val mock = init(ref(sym))
      (s"Node($id)[" ~ printer.toText(tpe) ~ "]{" ~ printer.toText(mock :: rest, "\n") ~ "}").close


  extension (body: List[Node])
    private def append(trees: List[Tree]) = body.init :+ body.last.append(trees)


  private case class Coroutine(head: List[Tree], body: List[Node]) extends printing.Showable:
    def prepend(trees: List[Tree]) = copy(head = trees ++ head)
    def append(trees: List[Tree]) = copy(body = body.append(trees))

    import printing.*, Texts._
    def toText(printer: Printer): Text =
      val headText: Text = ("Head{" ~ printer.toText(head, "\n") ~ "}").close
      (s"Coroutine{" ~ headText ~ printer.toText(body, "\n") ~ "}").close

  private object Coroutine:
    def suspension(tpe: Type, state: Int)(using Context): Coroutine =
      Coroutine(Nil, Node(r => r.asInstance(tpe), Nil, tpe, state) :: Nil)


  private type Trees = Tree | List[Tree]
  private type PreCoroutine = Coroutine | Trees

  private object PreCoroutine:
    def empty: PreCoroutine = List.empty[Tree]

  extension (trees: Trees)
    private def toList = trees match
      case t: Tree => t :: Nil
      case tt: List[Tree] => tt

    private def toTree(using Context) = trees match
      case t: Tree => t
      case tt: List[Tree] => Block(tt.init, tt.last)


  extension (left: PreCoroutine)
    private infix def combine(right: PreCoroutine): PreCoroutine = (left, right) match
      case (l: Coroutine, r: Coroutine) => Coroutine(head = l.head, l.body.append(r.head) ++ r.body)
      case (l: Coroutine, r: Trees) => l.append(r.toList)
      case (l: Trees, r: Coroutine) => r.prepend(l.toList)
      case (l: Trees, r: Trees) => l.toList ++ r.toList

    private def orOriginal(original: => Tree): PreCoroutine = left match
      case l: Coroutine => l
      case _ => original

    private def simplify(f: List[Tree] => Tree): PreCoroutine = left match
      case l: Coroutine => l
      case tt: Trees => f(tt.toList)

    private def simplifyOrWrap(f: Tree => Tree)(using Context): PreCoroutine = left match
      case l: Coroutine => l.mapLastNode(_.wrap(f))
      case tt: List[Tree] => tt.toTree.simplifyOrWrap(f)
      case t: Tree => f(t)

    private def mapLastNode(f: Node => Node): PreCoroutine = left match
      case l: Coroutine => l.copy(body = l.body.init :+ f(l.body.last))
      case t => t

    private def head: List[Tree] = left match
      case l: Coroutine => l.head
      case tt: Trees => tt.toList

    private def headless: PreCoroutine = left match
      case l: Coroutine => l.copy(head = Nil)
      case tt: Trees => Nil

    private def nodes: List[Node] = left match
      case l: Coroutine => l.body
      case tt: Trees => Nil
