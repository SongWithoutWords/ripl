name := "ripl"
version := "0.1.0-SNAPSHOT"

organization := "org.ripl"
scalaVersion := "2.12.6"
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest"  % "3.0.3" % Test,
  "com.beachape"  %% "enumeratum" % "1.5.12",
  "org.typelevel" %% "cats-core"  % "1.1.0"
)

scalacOptions ++= Seq(

  // warnings for deprecated api usage
  "-deprecation",

  // warnings for language features not enabled
  "-feature",

  // enable higher-kinded types
  "-language:higherKinds",

  // enable implicitConversions
  "-language:implicitConversions",

  // detailed unchecked warnings
  "-unchecked",

  // partial unification, required by cats
  "-Ypartial-unification",

  // warn when non-unit expression results are unused
  "-Ywarn-value-discard"
)

ensimeScalaVersion in ThisBuild := "2.12.6"
ensimeIgnoreMissingDirectories := true;

scalafmtOnCompile := true

enablePlugins(Antlr4Plugin)
antlr4GenListener in Antlr4 := false
antlr4GenVisitor in Antlr4 := false

// run tests in a separate JVM (avoids out-of-memory issues)
fork in Test := true

// suppress successful test messages
testOptions in Test += Tests.Argument("-oC")
logBuffered in Test := false

// val llvmBindings = RootProject(file("../scala-llvm-bindings"))
// dependsOn(llvmBindings)
