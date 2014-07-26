import sbt._
import Keys._

object BuildSettings {
  val myBuildSettings = Defaults.defaultSettings ++ Seq (
    name         := "datr.scala",
    version      := "0.1",
    organization := "platzhaltr",
    scalaVersion := "2.11.1",
    scalacOptions ++= Seq()
  )
}

object Dependencies {
  val jodaTime    = "joda-time" % "joda-time" % "2.3"
  val jodaConvert = "org.joda" % "joda-convert" % "1.6"
  val scalatest   = "org.scalatest" %% "scalatest" % "2.2.0" % "test"
  val parboiled   = "org.parboiled" %% "parboiled" % "2.0.0"
  val reflect     = "org.scala-lang" % "scala-reflect" % "2.11.1"

  val myDependencies = Seq(jodaTime, jodaConvert, scalatest, parboiled, reflect)
}

object MyBuild extends Build {
  import BuildSettings._
  import Dependencies._

  lazy val root: Project = Project(
    "root",
    file("."),
    settings =
      myBuildSettings ++
      Seq (libraryDependencies ++= myDependencies)
  )
}
