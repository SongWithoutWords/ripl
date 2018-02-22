import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.3"

  val enumeratumVersion = "1.5.12"
  lazy val enumeratum = "com.beachape" %% "enumeratum" % enumeratumVersion
}
