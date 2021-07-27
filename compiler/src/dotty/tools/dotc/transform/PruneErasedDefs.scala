package dotty.tools.dotc
package transform

import core._
import Contexts._
import DenotTransformers.SymTransformer
import Flags._
import SymDenotations._
import Symbols._
import Types._
import typer.RefChecks
import MegaPhase.MiniPhase
import StdNames.nme
import ast.tpd

/** This phase makes all erased term members of classes private so that they cannot
 *  conflict with non-erased members. This is needed so that subsequent phases like
 *  ResolveSuper that inspect class members work correctly.
 *  The phase also replaces all expressions that appear in an erased context by
 *  default values. This is necessary so that subsequent checking phases such
 *  as IsInstanceOfChecker don't give false negatives.
 */
class PruneErasedDefs extends MiniPhase with SymTransformer { thisTransform =>
  import tpd._
  import PruneErasedDefs._

  override def phaseName: String = PruneErasedDefs.name

  override def changesMembers: Boolean = true   // makes erased members private

  override def runsAfterGroupsOf: Set[String] = Set(RefChecks.name, ExplicitOuter.name)

  override def transformSym(sym: SymDenotation)(using Context): SymDenotation =
    if !sym.isEffectivelyErased || !sym.isTerm || sym.is(Private) || !sym.owner.isClass then sym
    else sym.copySymDenotation(initFlags = sym.flags | Private)

  override def transformApply(tree: Apply)(using Context): Tree =
    if !tree.fun.tpe.widen.isErasedMethod then tree
    else cpy.Apply(tree)(tree.fun, tree.args.map(trivialErasedTree))

  override def transformValDef(tree: ValDef)(using Context): Tree =
    if !tree.symbol.isEffectivelyErased || tree.rhs.isEmpty then tree
    else cpy.ValDef(tree)(rhs = trivialErasedTree(tree.rhs))

  override def transformDefDef(tree: DefDef)(using Context): Tree =
    if !tree.symbol.isEffectivelyErased || tree.rhs.isEmpty then tree
    else cpy.DefDef(tree)(rhs = trivialErasedTree(tree.rhs))

}

object PruneErasedDefs {
  import tpd._

  val name: String = "pruneErasedDefs"

  def trivialErasedTree(tree: Tree)(using Context): Tree =
    ref(defn.Compiletime_erasedValue).appliedToType(tree.tpe).withSpan(tree.span)
}
