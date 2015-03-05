enablePlugins(ScalaJSPlugin)

name         := "datr.scala"

version      := "0.1"

organization := "platzhaltr"

scalaVersion := "2.11.5"

libraryDependencies += "joda-time" % "joda-time" % "2.7"

libraryDependencies += "org.joda" % "joda-convert" % "1.7"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

libraryDependencies += "org.parboiled" %%% "parboiled" % "2.0.1"
