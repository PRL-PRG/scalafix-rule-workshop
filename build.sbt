organization := "cz.cvut.fit.prl"
name := "implicit-collector"

scalacOptions += "-Yno-adapted-args"
assemblyJarName in assembly := "implicit-collector.jar"

lazy val core = (project in file("core")).settings(
  scalaVersion := "2.12.4",
  libraryDependencies ++= Seq(
    "ch.qos.logback" % "logback-classic" % "1.2.3",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
    "org.scalameta" %% "testkit" % "2.1.2",
    "org.scalameta" % "semanticdb-scalac_2.12.4" % "2.1.2",
    "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    "com.github.scopt" % "scopt_2.12" % "3.7.0",
    "org.scalactic" %% "scalactic" % "3.0.4",
    "org.scalatest" %% "scalatest" % "3.0.4" % "test",
    "com.github.nikita-volkov" % "sext" % "0.2.4"
  )
)

lazy val plugin = (project in file("plugin")).dependsOn(core).settings(
  organization := "cz.cvut.fit.prl",
  name := "sbt-implicit-collector",
  version := "0.1",
  scalaVersion := "2.12.4",
  sbtPlugin := true,
  sbtVersion in Global := "1.0.0",
  moduleName := "sbt-implicit-collector"
)
