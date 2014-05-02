import sbt._
import Keys._

object BuildSettings {
  val myBuildSettings = Defaults.defaultSettings ++ Seq (
    name         := "parboiled",
    version      := "1.0",
    organization := "com.acme",
    scalaVersion := "2.11.0",
    scalacOptions ++= Seq()
  )
}

object Dependencies {
  val scalatest = "org.scalatest" %% "scalatest" % "2.1.4" % "test"
  val parboiled = "org.parboiled" %% "parboiled" % "2.0.0-RC1"

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