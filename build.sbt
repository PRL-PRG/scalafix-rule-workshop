organization := "cz.cvut.fit.prl"
name := "implicit-collector"
version := "0.1"
scalaVersion := "2.12.4"
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

sbtPlugin := true
//sbtVersion := "0.13.x"
scalacOptions += "-Yno-adapted-args"
assemblyJarName in assembly := "implicit-collector.jar"
