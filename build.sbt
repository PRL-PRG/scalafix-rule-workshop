name := "implicit-collector"
version := "0.1"
scalaVersion := "2.12.4"
libraryDependencies ++= Seq(
    "org.scalameta" %% "testkit" % "2.1.2",

  // not sure why we need the explicit _2.12.4
  // but it does not work with it
  // perhaps a question for Olaf
    "org.scalameta" % "semanticdb-scalac_2.12.4" % "2.1.2",
    "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    "com.github.scopt" % "scopt_2.12" % "3.7.0",
    "org.scalactic" %% "scalactic" % "3.0.4",
    "org.scalatest" %% "scalatest" % "3.0.4" % "test"
)

scalacOptions += "-Yno-adapted-args"
assemblyJarName in assembly := "implicit-collector.jar"