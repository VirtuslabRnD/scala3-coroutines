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

object Continuations:
  class Transform extends MiniPhase with IdentityDenotTransformer:
    thisPhase =>
    def phaseName = "continuations"

    override def transformApply(tree: Apply)(using Context): Tree = tree.fun match
      case TypeApply(Select(a, f), t::Nil) if tree.symbol.owner == defn.CoroutineExecutor && tree.symbol.name == nme.run =>
        val answerType = t.tpe.typeSymbol.asType.typeRef
        val stabilizer = newSymbol(ctx.owner, "$stabilizer".toTermName, Synthetic, a.tpe).entered
        val stabilizerVal = ValDef(stabilizer.asTerm, a)
        Block(stabilizerVal :: Nil, cpy.Apply(tree)(
          ref(stabilizer).select("process".toTermName).appliedToType(answerType),
          tree.args.map(procArgs(stabilizer, answerType))
        ))
      case _ => tree

    private def procArgs(stabilizer: TermSymbol, answerType: Type)(using Context): Tree => Tree =
      case b @ Block((d: DefDef) :: Nil, e) =>
        val coroutine = generateCoroutine(d.rhs, d.symbol.owner, stabilizer, answerType)
        Block(coroutine :: Nil, New(coroutine.symbol.asType.typeRef, Nil))
      case t =>
        t

    private def generateCoroutine(rhs: Tree, owner: Symbol, stabilizer: TermSymbol, answerType: Type)(using Context): TypeDef =
      val stabRef = TermRef(NoPrefix, stabilizer)
      val tpe = TypeRef(stabRef, "Coroutine".toTypeName).appliedTo(answerType)
      val cls = newNormalizedClassSymbol(owner, tpnme.ANON_CLASS, Synthetic | Final, tpe :: Nil)
      val constr = DefDef(newConstructor(cls, Synthetic, Nil, Nil).entered)

      def tref(name: String) = TypeRef(cls.thisType, name.toTypeName)
      val stateT = tref("State")
      val stateIdT = tref("StateId")
      val extractT = TypeRef(stabRef, "Extract".toTypeName)
      def makeId(id: Int): Tree = Literal(Constant(id))//.asInstance(stateIdT)

      given lifter: LiftState = LiftState(cls)
      val analysis = analyze(rhs).simplifyOrWrap(lifter.markFinish)
      val vars = lifter.symbols.map(s => ValDef(s))


      val gotoSym = newSymbol(
        owner = cls,
        name = "goto".toTermName,
        flags = Method | Private | Synthetic,
        info = MethodType(("stateId" :: "t" :: Nil).map(_.toTermName), stateIdT :: extractT :: Nil, OrType.make(stateT, stateIdT, false))
      ).asTerm.entered

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
        info = MethodType(("stateId" :: "t" :: Nil).map(_.toTermName), stateIdT :: extractT :: Nil, stateT)
      ).asTerm.entered

      def dispatchRhs(argss: List[List[Tree]]): Tree =
        def makeCase(name: String, tpe: Type)(body: Tree => Tree) =
          val sym = newSymbol(dispatchSym, name.toTermName, Case | Synthetic, tpe)
          CaseDef(Bind(sym, Typed(Underscore(tpe), TypeTree(tpe))), EmptyTree, body(ref(sym)))

        val (idRef :: extractRef :: Nil) :: Nil = argss: @unchecked
        val stCase = makeCase("st", stateT)(identity)
        val idCase = makeCase("id", stateIdT)(ref(dispatchSym).appliedTo(_, Literal(Constant(null)).asInstance(extractT)))
        Match(ref(gotoSym).appliedTo(idRef, extractRef), stCase :: idCase :: Nil)
      end dispatchRhs

      val dispatchTree = DefDef(dispatchSym, dispatchRhs)


      val startSym = defn.Coroutine_start.copy(
        owner = cls,
        info = MethodType(Nil, Nil, stateT),
        flags = Synthetic | Method | Final | Override
      ).asTerm.entered
      val startRhs = ref(dispatchSym).appliedTo(Literal(Constant(0)), Literal(Constant(null)).asInstance(extractT))
      val startTree = DefDef(startSym, startRhs)

      val resumeSym = defn.Coroutine_resume.copy(
        owner = cls,
        info = MethodType(("stateId" :: "t" :: Nil).map(_.toTermName), stateIdT :: extractT :: Nil, stateT),
        flags = Synthetic | Method | Final | Override
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
          case v: ValDef => v.symbol.copySymDenotation(owner = newOwner).installAfter(thisPhase)
          case _ => traverseChildren(tree)
      Reowner.traverse(block)


  /* === analysis === */

  private class LiftState(cls: ClassSymbol)(using Context):
    private val stateMod = This(cls).select("State".toTermName)
    private val finished = stateMod.select("Finished".toTermName).select(nme.apply)
    private val progressed = stateMod.select("Progressed".toTermName).select(nme.apply)

    private var states: Int = 0
    private var counter: Int = 0
    var symbols: List[TermSymbol] = Nil
    val lifted = mutable.Map[Symbol, Symbol]()

    def last = symbols.head

    def make(tpe: Type, original: Symbol = NoSymbol)(using Context): TermSymbol =
      val suffix = if original == NoSymbol then "" else "_$" + original.name.toString
      val sym = newSymbol(
        owner = cls,
        name = ("$lift_" + counter + suffix).toTermName,
        flags = Synthetic | Private | Mutable,
        info = tpe.widen
      ).entered.asTerm
      counter += 1
      symbols = sym :: symbols
      if original != NoSymbol then lifted += (original -> sym)
      sym

    def newState =
      states += 1
      states

    def markProgress(to: Int)(tree: Tree): Tree =
      progressed.appliedTo(tree, Literal(Constant(to)))

    def markFinish(tree: Tree): Tree = finished.appliedTo(tree)
  end LiftState

  private def lifter(using l: LiftState) = l

  private def analyze(tree: Tree)(using LiftState, Context): PreCoroutine = tree match
    // blocks:
    case Block(stats, expr) => (stats :+ expr).map(analyze).reduce(_ combine _).simplify(tt => cpy.Block(tree)(tt.init, tt.last))
    case i: Inlined => analyze(Inliner.dropInlined(i))

    // simple wrappable:
    case Typed(expr, tpt) => analyze(expr).simplifyOrWrap(cpy.Typed(tree)(_, tpt))
    case Select(qual, name) => analyze(qual).simplifyOrWrap(cpy.Select(tree)(_, name))
    case TypeApply(fn, args) => analyze(fn).simplifyOrWrap(cpy.TypeApply(tree)(_, args))

    // always lifted:
    case v: ValDef =>
      val sym = lifter.make(v.rhs.tpe, v.symbol)
      analyze(v.rhs).simplifyOrWrap(t => cpy.Assign(v)(lhs = ref(sym), rhs = t))

    // suspend - create the new coroutine:
    case Apply(call, arg :: Nil) if call.symbol.owner == defn.ContinuationContext && call.symbol.name.toString == "suspend" =>
      val newState = lifter.newState
      val left = analyze(arg).simplifyOrWrap(lifter.markProgress(newState))
      left combine Coroutine(Nil, Node(r => r.asInstance(tree.tpe), Nil, tree.tpe, newState) :: Nil)

    // trees that can suspend in different subtrees:
    case Apply(call, args) =>
      val callLift = analyzeWithLift(call)
      val argLifts = args.map(analyzeWithLift)
      val precoroutine = (callLift :: argLifts).collect { case Lift(Some(c), _) => c }.fold[PreCoroutine](List.empty[Tree])(_ combine _)
      (precoroutine combine cpy.Apply(tree)(fun = callLift.tree, args = argLifts.map(_.tree)))

    case SeqLiteral(elems, tpe) =>
      val lifts = elems.map(analyzeWithLift)
      val precoroutine = lifts.collect {case Lift(Some(c), _) => c}.fold[PreCoroutine](List.empty[Tree])(_ combine _)
      (precoroutine combine cpy.SeqLiteral(tree)(lifts.map(_.tree), tpe))

    // trees that can branch and needs more complex logic:
    case If(cond, thenp, elsep) =>
      val condLift = analyzeWithLift(cond)
      val tail = (analyze(thenp), analyze(elsep)) match
        case (thent: Trees, elset: Trees) => cpy.If(tree)(condLift.tree, thent.toTree, elset.toTree)
        case (thenpc, elsepc) =>
          val newState = lifter.newState
          val sym = lifter.make(tree.tpe)
          def lift(pc: PreCoroutine) = pc.simplifyOrWrap(Assign(ref(sym), _)) combine Literal(Constant(newState))
          val thenl = lift(thenpc)
          val elsel = lift(elsepc)
          cpy.If(tree)(condLift.tree, thenl.head.toTree, elsel.head.toTree) combine
            thenl.body combine
            elsel.body combine
            Coroutine(Nil, Node(_ => ref(sym), Nil, tree.tpe, newState) :: Nil)

      condLift.coroutine match
        case Some(head) => head combine tail
        case None => tail

    // may be lcoal reference that was lifted
    case t: Ident =>
      lifter.lifted.get(t.symbol) match
        case Some(symbol) => ref(symbol)
        case None => t

    // do not need transformations:
    case _: Literal  => tree

    // debug case TODO: remove
    case _ =>
      println("\n\n\t====")
      println(tree.show)
      println(tree)
      // System.exit(-1)
      tree


  private case class Lift(coroutine: Option[Coroutine], tree: Tree)

  private def analyzeWithLift(tree: Tree)(using LiftState, Context): Lift =
    analyze(tree) match
      case t: Tree => Lift(None, t)
      case tt: List[Tree] => Lift(None, tt.toTree)
      case coroutine: Coroutine =>
        val c = coroutine.mapLastNode { n =>
          val sym = lifter.make(n.tpe)
          n.wrap(Assign(ref(sym), _))
        }.asInstanceOf[Coroutine]
        Lift(Some(c), ref(lifter.last)) // TODO: Eliminate last


  /* === coroutines building blocks === */

  private case class Node(init: Tree => Tree, rest: List[Tree], tpe: Type, id: Int):
    def append(trees: List[Tree]) = if trees.nonEmpty then copy(rest = rest ++ trees, tpe = trees.last.tpe) else this

    def wrap(f: Tree => Tree)(using Context): Node =
      if rest.isEmpty then copy(init = r => f(init(r)))
      else copy(rest = f(Block(rest.init, rest.last)) :: Nil)


  extension (body: List[Node])
    private def append(trees: List[Tree]) = body.init :+ body.last.append(trees)


  private case class Coroutine(head: List[Tree], body: List[Node]):
    def prepend(trees: List[Tree]) = copy(head = trees ++ head)
    def append(trees: List[Tree]) = copy(body = body.append(trees))


  private type Trees = Tree | List[Tree]
  private type PreCoroutine = Coroutine | Trees

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

    private def body: PreCoroutine = left match
      case l: Coroutine => l.copy(head = Nil)
      case tt: Trees => Nil

    private def nodes: List[Node] = left match
      case l: Coroutine => l.body
      case tt: Trees => Nil

