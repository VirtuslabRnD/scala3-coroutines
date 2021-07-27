package dotty.tools
package dotc
package config

import core._
import Contexts._, Symbols._, Names._, NameOps._, Phases._
import StdNames.nme
import Decorators.{_, given}
import util.{SrcPos, NoSourcePosition}
import SourceVersion._
import reporting.Message
import NameKinds.QualifiedName

object Feature:

  def experimental(str: PreName): TermName =
    QualifiedName(nme.experimental, str.toTermName)

  private def deprecated(str: PreName): TermName =
    QualifiedName(nme.deprecated, str.toTermName)

  private val namedTypeArguments = experimental("namedTypeArguments")
  private val genericNumberLiterals = experimental("genericNumberLiterals")
  val scala2macros = experimental("macros")

  val dependent = experimental("dependent")
  val erasedDefinitions = experimental("erasedDefinitions")
  val symbolLiterals = deprecated("symbolLiterals")
  val fewerBraces = experimental("fewerBraces")

  /** Is `feature` enabled by by a command-line setting? The enabling setting is
   *
   *       -language:<prefix>feature
   *
   *  where <prefix> is the fully qualified name of `owner`, followed by a ".",
   *  but subtracting the prefix `scala.language.` at the front.
   */
  def enabledBySetting(feature: TermName)(using Context): Boolean =
    ctx.base.settings.language.value.contains(feature.toString)

  /** Is `feature` enabled by by an import? This is the case if the feature
   *  is imported by a named import
   *
   *       import owner.feature
   *
   *  and there is no visible nested import that excludes the feature, as in
   *
   *       import owner.{ feature => _ }
   */
  def enabledByImport(feature: TermName)(using Context): Boolean =
    //atPhase(typerPhase) {
      ctx.importInfo != null && ctx.importInfo.featureImported(feature)
    //}

  /** Is `feature` enabled by either a command line setting or an import?
   *  @param  feature   The name of the feature
   *  @param  owner     The prefix symbol (nested in `scala.language`) where the
   *                    feature is defined.
   */
  def enabled(feature: TermName)(using Context): Boolean =
    enabledBySetting(feature) || enabledByImport(feature)

  /** Is auto-tupling enabled? */
  def autoTuplingEnabled(using Context): Boolean = !enabled(nme.noAutoTupling)

  def dynamicsEnabled(using Context): Boolean = enabled(nme.dynamics)

  def dependentEnabled(using Context) = enabled(dependent)

  def namedTypeArgsEnabled(using Context) = enabled(namedTypeArguments)

  def genericNumberLiteralsEnabled(using Context) = enabled(genericNumberLiterals)

  def scala2ExperimentalMacroEnabled(using Context) = enabled(scala2macros)

  def sourceVersionSetting(using Context): SourceVersion =
    SourceVersion.valueOf(ctx.settings.source.value)

  def sourceVersion(using Context): SourceVersion =
    if ctx.compilationUnit == null then sourceVersionSetting
    else ctx.compilationUnit.sourceVersion match
      case Some(v) => v
      case none => sourceVersionSetting

  def migrateTo3(using Context): Boolean = sourceVersion == `3.0-migration`

  /** If current source migrates to `version`, issue given warning message
   *  and return `true`, otherwise return `false`.
   */
  def warnOnMigration(msg: Message, pos: SrcPos,
      version: SourceVersion = defaultSourceVersion)(using Context): Boolean =
    if sourceVersion.isMigrating && sourceVersion.stable == version
       || version == `3.0` && migrateTo3
    then
      report.migrationWarning(msg, pos)
      true
    else
      false

  private val assumeExperimentalIn = Set("dotty.tools.vulpix.ParallelTesting", "dotty.tools.dotc.BootstrappedStdLibTASYyTest")

  def checkExperimentalFeature(which: String, srcPos: SrcPos)(using Context) =
    if !isExperimentalEnabled then
      report.error(i"Experimental $which may only be used with a nightly or snapshot version of the compiler", srcPos)

  def checkExperimentalDef(sym: Symbol, srcPos: SrcPos)(using Context) =
    if !isExperimentalEnabled then
      val symMsg =
        if sym eq defn.ExperimentalAnnot then
          i"use of @experimental is experimental"
        else if sym.hasAnnotation(defn.ExperimentalAnnot) then
          i"$sym is marked @experimental"
        else if sym.owner.hasAnnotation(defn.ExperimentalAnnot) then
          i"${sym.owner} is marked @experimental"
        else
          i"$sym inherits @experimental"
      report.error(s"$symMsg and therefore may only be used with a nightly or snapshot version of the compiler", srcPos)

  /** Check that experimental compiler options are only set for snapshot or nightly compiler versions. */
  def checkExperimentalSettings(using Context): Unit =
    for setting <- ctx.settings.language.value
        if setting.startsWith("experimental.") && setting != "experimental.macros"
    do checkExperimentalFeature(s"feature $setting", NoSourcePosition)

  def isExperimentalEnabled(using Context): Boolean =
    def hasSpecialPermission =
      Thread.currentThread.getStackTrace.exists(elem =>
        assumeExperimentalIn.exists(elem.getClassName().startsWith(_)))
    (Properties.experimental || hasSpecialPermission) && !ctx.settings.YnoExperimental.value

end Feature