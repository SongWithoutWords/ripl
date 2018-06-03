name      := "ripl"
version   := "0.1.0-SNAPSHOT"

organization := "org.ripl"
scalaVersion := "2.12.6"
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.3" % Test,
  "com.beachape" %% "enumeratum" % "1.5.12"
)

scalafmtOnCompile := true

enablePlugins(Antlr4Plugin)
antlr4GenListener in Antlr4 := false
antlr4GenVisitor in Antlr4 := false

// suppress successful test messages
testOptions in Test += Tests.Argument("-oC")
logBuffered in Test := false

