import sbt._
import Keys._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

object BuildSettings {
  val myBuildSettings = Defaults.coreDefaultSettings ++ Seq (
    name         := "datr.scala",
    version      := "0.1",
    organization := "platzhaltr",
    scalaVersion := "2.11.5",
    scalacOptions ++= Seq()
  )
}

object Dependencies {
  val jodaTime    = "joda-time" % "joda-time" % "2.7"
  val jodaConvert = "org.joda" % "joda-convert" % "1.7"
  val scalatest   = "org.scalatest" %% "scalatest" % "2.2.4" % "test"
    val parboiled   = "org.parboiled" %% "parboiled" % "2.0.1"
  val reflect     = "org.scala-lang" % "scala-reflect" % "2.11.5"

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
  ) enablePlugins(ScalaJSPlugin)
}
