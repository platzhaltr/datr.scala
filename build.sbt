name         := "datr.scala"

version      := "0.1.0"

organization := "org.platzhaltr.parsing"

scalaVersion := "2.13.2"

libraryDependencies += "org.threeten" % "threeten-extra" % "1.5.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"

libraryDependencies += "org.parboiled" %% "parboiled" % "2.2.0"

scalacOptions ++= Seq(
    "-target:jvm-1.8",
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-language:experimental.macros",
    "-unchecked",
    "-Xlint",
    "-Ywarn-dead-code")

conflictManager := ConflictManager.strict

bintrayOmitLicense := true
