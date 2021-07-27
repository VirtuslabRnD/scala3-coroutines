val scala3 = Build.scala3
val `scala3-bootstrapped` = Build.`scala3-bootstrapped`
val `scala3-interfaces` = Build.`scala3-interfaces`
val `scala3-compiler` = Build.`scala3-compiler`
val `scala3-compiler-bootstrapped` = Build.`scala3-compiler-bootstrapped`
val `scala3-library` = Build.`scala3-library`
val `scala3-library-bootstrapped` = Build.`scala3-library-bootstrapped`
val `scala3-library-bootstrappedJS` = Build.`scala3-library-bootstrappedJS`
val `scala3-library-cont` = Build.`scala3-library-cont`
val `scala3-sbt-bridge` = Build.`scala3-sbt-bridge`
val `scala3-sbt-bridge-tests` = Build.`scala3-sbt-bridge-tests`
val `scala3-staging` = Build.`scala3-staging`
val `scala3-tasty-inspector` = Build.`scala3-tasty-inspector`
val `scala3-language-server` = Build.`scala3-language-server`
val `scala3-bench` = Build.`scala3-bench`
val `scala3-bench-bootstrapped` = Build.`scala3-bench-bootstrapped`
val `stdlib-bootstrapped` = Build.`stdlib-bootstrapped`
val `stdlib-bootstrapped-tasty-tests` = Build.`stdlib-bootstrapped-tasty-tests`
val `tasty-core` = Build.`tasty-core`
val `tasty-core-bootstrapped` = Build.`tasty-core-bootstrapped`
val `tasty-core-scala2` = Build.`tasty-core-scala2`
val scaladoc = Build.scaladoc
val `scaladoc-testcases` = Build.`scaladoc-testcases`
val `scaladoc-js` = Build.`scaladoc-js`
val `scala3-bench-run` = Build.`scala3-bench-run`
val dist = Build.dist
val `community-build` = Build.`community-build`
val `sbt-community-build` = Build.`sbt-community-build`

val sjsSandbox = Build.sjsSandbox
val sjsJUnitTests = Build.sjsJUnitTests
val sjsCompilerTests = Build.sjsCompilerTests

val `sbt-test` = Build.`sbt-test`
val `vscode-dotty` = Build.`vscode-dotty`

inThisBuild(Build.thisBuildSettings)
inScope(Global)(Build.globalSettings)
