import sbt._
import Keys._

object BuildSettings {
  val myBuildSettings = Defaults.defaultSettings ++ Seq (
    name         := "parboiled",
    version      := "0.1",
    organization := "com.acme",
    scalaVersion := "2.11.1",
    scalacOptions ++= Seq()
  )
}

object Dependencies {
  val scalatest = "org.scalatest" %% "scalatest" % "2.2.0" % "test"
  val parboiled = "org.parboiled" %% "parboiled" % "2.0.0"

  val myDependencies = Seq(scalatest, parboiled)
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
