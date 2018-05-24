import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.ripl",
      scalaVersion := "2.12.3",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "ripl",
    libraryDependencies ++= Seq(
      scalaTest % Test,
      enumeratum
    )
  )

enablePlugins(Antlr4Plugin)

antlr4GenListener in Antlr4 := true
antlr4GenVisitor in Antlr4 := true

