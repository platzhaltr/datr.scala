enablePlugins(ScalaJSPlugin)

name         := "datr.scala"

version      := "0.1"

organization := "platzhaltr"

scalaVersion := "2.11.7"

resolvers ++= Seq(
  "snapshots"           at "https://oss.sonatype.org/content/repositories/snapshots")

libraryDependencies += "org.mdedetrich" %% "soda-time" % "0.0.1-SNAPSHOT"

libraryDependencies += "org.joda" % "joda-convert" % "1.7"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

libraryDependencies += "org.parboiled" %%% "parboiled" % "2.1.0"

