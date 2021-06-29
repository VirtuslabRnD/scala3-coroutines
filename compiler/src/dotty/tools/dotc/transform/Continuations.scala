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
import dotty.tools.dotc.core.Names.TermName
import dotty.tools.dotc.core.Names.TermName

object Continuations:
  class Transform extends MiniPhase with IdentityDenotTransformer:
    thisPhase =>
    def phaseName = "continuations"

    override def transformDefDef(tree: DefDef)(using Context) =
      val orig = tree.symbol.asTerm
      val coroutine = Option.unless(orig.is(Inline))(orig.info).flatMap(findContext).map { t =>
        val sym = orig.copy(
          name = orig.name ++ "$coroutine",
          info = t.makeType,
          flags = orig.flags | Synthetic
        ).asTerm.entered

        def rhs(paramss: List[List[Tree]]) =
          val mapping: Map[Symbol, Tree] = orig.paramSymss.zip(paramss).flatMap(_ zip _).toMap
          val stabilizer = TermRef(NoPrefix, paramss.last.last.symbol)
          procClosure(stabilizer, t.result, mapping)(tree.rhs)

        val result = DefDef(sym, rhs)
        reown(result.rhs, sym)
        result
      }
      coroutine match
        case Some(c) => Thicket(c, tree)
        case _ => tree

    case class CInfo(namess: List[List[TermName]], argss: List[List[Type]], executor: Type, result: Type):
      def makeType(using Context) =
        def rec(namess: List[List[TermName]], argss: List[List[Type]], executor: Type): Type = (namess, argss) match
          case (names :: nr, args :: ar) => MethodType(names, args, rec(nr, ar, executor))
          case (_, _) =>
            MethodType("$executor".toTermName :: Nil)(_ => executor::Nil, m => m.newParamRef(0).select("Coroutine".toTypeName).appliedTo(result))
        rec(namess, argss, executor)

    private def findContext(tpe: Type)(using Context): Option[CInfo] = tpe match
      case MethodTpe(names, tpes, ret) => findContext(ret).map(i => i.copy(names :: i.namess, tpes :: i.argss))
      case defn.ContextFunctionType(params, res, _) =>
        params.find(_.classSymbol == defn.ContinuationContext)
          .collect { case TypeRef(exe, _) => exe }
          .map(CInfo(Nil, Nil, _, res))
      case _ => None

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
      case t =>
        t

    private def generateCoroutine(rhs: Tree, owner: Symbol, typePrefix: NamedType, answerType: Type, params: Map[Symbol, Tree])(using Context): TypeDef =
      val tpe = TypeRef(typePrefix, "Coroutine".toTypeName).appliedTo(answerType)
      val cls = newNormalizedClassSymbol(owner, tpnme.ANON_CLASS, Synthetic | Final, tpe :: Nil)
      val constr = DefDef(newConstructor(cls, Synthetic, Nil, Nil).entered)

      val stackChT = TypeRef(typePrefix, "StackChange".toTypeName)
      val extractT = TypeRef(typePrefix, "Extract".toTypeName)
      def makeId(id: Int): Tree = Literal(Constant(id))

      given lifter: LiftState = LiftState(typePrefix, cls, params)
      val analysis = analyze(rhs).simplifyOrWrap(lifter.markStackPop)
      val vars = lifter.symbols.map(s => ValDef(s))


      val gotoSym = newSymbol(
        owner = cls,
        name = "goto".toTermName,
        flags = Method | Private | Synthetic,
        info = MethodType(("stateId" :: "t" :: Nil).map(_.toTermName), defn.IntType :: extractT :: Nil, OrType.make(stackChT, defn.IntType, false))
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
        info = MethodType(("stateId" :: "t" :: Nil).map(_.toTermName), defn.IntType :: extractT :: Nil, stackChT)
      ).asTerm.entered

      def dispatchRhs(argss: List[List[Tree]]): Tree =
        def makeCase(name: String, tpe: Type)(body: Tree => Tree) =
          val sym = newSymbol(dispatchSym, name.toTermName, Case | Synthetic, tpe)
          CaseDef(Bind(sym, Typed(Underscore(tpe), TypeTree(tpe))), EmptyTree, body(ref(sym)))

        val (idRef :: extractRef :: Nil) :: Nil = argss: @unchecked
        val stCase = makeCase("st", stackChT)(identity)
        val idCase = makeCase("id", defn.IntType)(ref(dispatchSym).appliedTo(_, Literal(Constant(null)).asInstance(extractT)))
        Match(ref(gotoSym).appliedTo(idRef, extractRef), stCase :: idCase :: Nil)
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

  private class LiftState(typePrefix: NamedType, cls: ClassSymbol, params: Map[Symbol, Tree])(using Context):
    private val stackChMod = ref(typePrefix).select("StackChange".toTermName)
    private val stackPop = stackChMod.select("Pop".toTermName).select(nme.apply)
    private val progress = stackChMod.select("Progress".toTermName).select(nme.apply)

    private var states: Int = 0
    private var counter: Int = 0
    var symbols: List[TermSymbol] = Nil
    private val lifted = mutable.Map[Symbol, Symbol]()

    def last = symbols.head

    def make(tpe: Type, original: Symbol = NoSymbol)(using Context): TermSymbol =
      val suffix = if original == NoSymbol then "" else "_$" + original.name.toString
      val sym = newSymbol(
        owner = cls,
        name = ("$lift_" + counter + suffix).toTermName,
        flags = Synthetic | Private | Mutable,
        info = tpe.widenUnion
      ).entered.asTerm
      counter += 1
      symbols = sym :: symbols
      if original != NoSymbol then lifted += (original -> sym)
      sym

    def translateRef(tree: Tree): Tree =
      params.get(tree.symbol) orElse lifted.get(tree.symbol).map(ref(_)) getOrElse tree

    def newState =
      states += 1
      states

    def markProgress(to: Int)(tree: Tree): Tree =
      progress.appliedTo(tree, Literal(Constant(to)))

    def markStackPop(tree: Tree): Tree = stackPop.appliedTo(tree)
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
      val precoroutine = (callLift :: argLifts).collect { case Lift(Some(c), _) => c }.fold(PreCoroutine.empty)(_ combine _)
      (precoroutine combine cpy.Apply(tree)(fun = callLift.tree, args = argLifts.map(_.tree)))

    case SeqLiteral(elems, tpe) =>
      val lifts = elems.map(analyzeWithLift)
      val precoroutine = lifts.collect { case Lift(Some(c), _) => c }.fold(PreCoroutine.empty)(_ combine _)
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
      lifter.translateRef(t)

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

    private def body: PreCoroutine = left match
      case l: Coroutine => l.copy(head = Nil)
      case tt: Trees => Nil

    private def nodes: List[Node] = left match
      case l: Coroutine => l.body
      case tt: Trees => Nil

