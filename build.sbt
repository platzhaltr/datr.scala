name         := "datr.scala"

version      := "0.1"

organization := "platzhaltr"

scalaVersion := "2.12.2"

libraryDependencies += "org.threeten" % "threeten-extra" % "1.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.3" % "test"

libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.4"

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
    //"-Ywarn-unused-import",
    "-Ywarn-nullary-unit",
    "-Xlint",
    //"-Yinline-warnings",
    "-Ywarn-dead-code",
    "-Xfuture")
