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
      case Select(a, f) if tree.symbol.owner == defn.CoroutineExecutor && tree.symbol.name == nme.run =>
        val stabilizer = newSymbol(ctx.owner, "$stabilizer".toTermName, Synthetic, a.tpe).entered
        val stabilizerVal = ValDef(stabilizer.asTerm, a)
        Block(stabilizerVal :: Nil, cpy.Apply(tree)(Select(ref(stabilizer), "process".toTermName), tree.args.map(procArgs(stabilizer))))
      case _ => tree

    private def procArgs(stabilizer: TermSymbol)(using Context): Tree => Tree =
      case b @ Block((d: DefDef) :: Nil, e) =>
        val coroutine = generateCoroutine(d.rhs, d.symbol.owner, stabilizer)
        Block(coroutine :: Nil, New(coroutine.symbol.asType.typeRef, Nil))
      case t =>
        t

    private def generateCoroutine(rhs: Tree, owner: Symbol, stabilizer: TermSymbol)(using Context): TypeDef =
      val stabRef = TermRef(NoPrefix, stabilizer)
      val tpe = TypeRef(stabRef, "Coroutine".toTypeName)
      val cls = newNormalizedClassSymbol(owner, tpnme.ANON_CLASS, Synthetic | Final, tpe :: Nil)
      val constr = DefDef(newConstructor(cls, Synthetic, Nil, Nil).entered)

      def tref(name: String) = TypeRef(cls.thisType, name.toTypeName)
      val stateT = tref("State")
      val stateIdT = tref("StateId")
      val extractT = TypeRef(stabRef, "Extract".toTypeName)
      def makeId(id: Int): Tree = Literal(Constant(id))//.asInstance(stateIdT)

      val stateMod = This(cls).select("State".toTermName)
      val finished = stateMod.select("Finished".toTermName).select(nme.apply)
      val progressed = stateMod.select("Progressed".toTermName).select(nme.apply)

      given lifter: Lifter = Lifter(cls)
      val analysis = analyze(rhs)
      val vars = lifter.symbols.map(s => ValDef(s))

      val startSym = defn.Coroutine_start.copy(
        owner = cls,
        info = MethodType(Nil, Nil, stateT),
        flags = Synthetic | Method | Final | Override
      ).asTerm.entered

      val startRhs = analysis match
        case tt: Trees => finished.appliedTo(tt.toBlock)
        case c: Coroutine => progressed.appliedToArgs(c.head.toBlock :: makeId(0) :: Nil)
      reown(startRhs, startSym)
      val startTree = DefDef(startSym, startRhs)

      val resumeSym = defn.Coroutine_resume.copy(
        owner = cls,
        info = MethodType(("stateId" :: "t" :: Nil).map(_.toTermName), stateIdT :: extractT :: Nil, stateT),
        flags = Synthetic | Method | Final | Override
      ).asTerm.entered

      def resumeRhs(argss: List[List[Tree]]): Tree =
        val idRef = argss(0)(0)
        val extractRef = argss(0)(1)
        val states = analysis match
          case c: Coroutine =>
            val indexed = c.body.zipWithIndex
            val init = indexed.init.map { (node, id) =>
              CaseDef(makeId(id), EmptyTree,
                progressed.appliedToArgs((node.init(extractRef) :: node.rest).toBlock :: makeId(id + 1) :: Nil))
            }
            val (fnode, fid) = indexed.last
            val last = CaseDef(makeId(fid), EmptyTree, finished.appliedTo((fnode.init(extractRef):: fnode.rest).toBlock))
            init :+ last
          case _ => Nil
        val fallback = Throw(New(defn.IllegalArgumentExceptionType, Nil))
        Match(idRef, states :+ CaseDef(Underscore(idRef.tpe), EmptyTree, fallback))

      val resumeTree = DefDef(resumeSym, resumeRhs)
      reown(resumeTree.rhs, resumeSym)

      val supertype = New(tpe, Nil)

      ClassDefWithParents(cls, constr, supertype::Nil, startTree :: resumeTree :: vars)
    end generateCoroutine

    private def reown(block: Tree, newOwner: Symbol)(using Context) =
      object Reowner extends TreeTraverser:
        override def traverse(tree: Tree)(using Context) = tree match
          case v: ValDef => v.symbol.copySymDenotation(owner = newOwner).installAfter(thisPhase)
          case _ => traverseChildren(tree)
      Reowner.traverse(block)


  /* === analysis === */

  private class Lifter(cls: ClassSymbol):
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
        info = tpe
      ).entered.asTerm
      counter += 1
      symbols = sym :: symbols
      if original != NoSymbol then lifted += (original -> sym)
      sym
  end Lifter

  private def analyze(tree: Tree)(using Lifter, Context): PreCoroutine = tree match
    // blocks:
    case Block(stats, expr) => (stats :+ expr).map(analyze).reduce(_ combine _).simplify(tt => cpy.Block(tree)(tt.init, tt.last))
    case i: Inlined => analyze(Inliner.dropInlined(i))

    // simple wrappable:
    case Typed(expr, tpt) => analyze(expr).simplifyOrWrap(t => cpy.Typed(tree)(t, tpt))
    case Select(qual, name) => analyze(qual).simplifyOrWrap(t => cpy.Select(tree)(t, name))

    // always lifted:
    case v: ValDef =>
      val sym = summon[Lifter].make(v.rhs.tpe, v.symbol)
      //analyze(v.rhs).mapLast(_.wrap(t => cpy.ValDef(v)(rhs = t))).orOriginal(tree)
      analyze(v.rhs).simplifyOrWrap(t => cpy.Assign(v)(lhs = ref(sym), rhs = t))

    // suspend, the only tree that can create new coroutine:
    case Apply(call, arg :: Nil) if call.symbol.owner == defn.ContinuationContext && call.symbol.name.toString == "suspend" =>
      analyze(arg) combine Coroutine(Nil, Node(r => r.asInstance(tree.tpe), Nil, tree.tpe) :: Nil)

    // trees that can branch and needs more complex logic:
    case Apply(call, args) =>
      val callLift = analyzeWithLift(call)
      val argLifts = args.map(analyzeWithLift)
      val precoroutine = (callLift :: argLifts).collect { case Lift(Some(c), _) => c }.fold[PreCoroutine](List.empty[Tree])(_ combine _)
      (precoroutine combine cpy.Apply(tree)(fun = callLift.tree, args = argLifts.map(_.tree)))

    // may be lcoal reference that was lifted
    case t: Ident =>
      summon[Lifter].lifted.get(t.symbol) match
        case Some(symbol) => ref(symbol)
        case None => t

    // do not need transformations:
    case _: Literal  => tree

    // debug case TODO: remove
    case _ =>
      println("\n\n\t====")
      println(tree.show)
      println(tree)
      tree


  private case class Lift(coroutine: Option[Coroutine], tree: Tree)

  private def analyzeWithLift(tree: Tree)(using Lifter, Context): Lift =
    analyze(tree) match
      case t: Tree => Lift(None, t)
      case tt: List[Tree] => Lift(None, tt.toBlock)
      case coroutine: Coroutine =>
        val lifter = summon[Lifter]
        val c = coroutine.mapLast { n =>
          val sym = lifter.make(n.tpe)
          n.wrap(Assign(ref(sym), _))
        }.asInstanceOf[Coroutine]
        Lift(Some(c), ref(lifter.last)) // TODO: Eliminate last


  /* === croutines building blocks === */

  private case class Node(init: Tree => Tree, rest: List[Tree], tpe: Type):
    def append(trees: List[Tree]) = copy(rest = rest ++ trees, tpe = trees.last.tpe)

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

    private def toBlock(using Context) =
      val tt = trees.toList
      Block(tt.init, tt.last)


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
      case l: Coroutine => l.mapLast(_.wrap(f))
      case tt: List[Tree] => tt.toBlock.simplifyOrWrap(f)
      case t: Tree => f(t)

    private def mapLast(f: Node => Node): PreCoroutine = left match
      case l: Coroutine => l.copy(body = l.body.init :+ f(l.body.last))
      case t => t
