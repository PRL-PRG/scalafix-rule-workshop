import sbt.Keys.name
name := "implicit-collector"

lazy val commonSettings = Seq(
  organization := "cz.cvut.fit.prl",
  version := "0.1",
  scalaVersion := "2.12.4",
  sbtPlugin := true,
  //sbtVersion := "0.13.x"
  scalacOptions += "-Yno-adapted-args"
)

lazy val root = (project in file("."))
  .aggregate(coreutils, macros, queries)
  .settings(commonSettings: _*)

lazy val coreutils = (project in file("core"))
  .settings(commonSettings: _*)
  .settings(
    assemblyJarName in assembly := "implicit-collector.jar",
    libraryDependencies ++= Seq(
      "ch.qos.logback" % "logback-classic" % "1.2.3",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
      "org.scalameta" %% "testkit" % "2.1.2",
      "org.scalameta" % "semanticdb-scalac_2.12.4" % "2.1.2",
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "com.github.scopt" % "scopt_2.12" % "3.7.0",
      "org.scalactic" %% "scalactic" % "3.0.4",
      "org.scalatest" %% "scalatest" % "3.0.4" % "test",
      "com.github.nikita-volkov" % "sext" % "0.2.4",
      "io.suzaku" %% "boopickle" % "1.2.6"
    )
  )
lazy val macros = (project in file("macros"))
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )
  .dependsOn(coreutils)

lazy val queries = (project in file("queries"))
  .settings(commonSettings: _*)
  .dependsOn(coreutils % "test->test", macros)
