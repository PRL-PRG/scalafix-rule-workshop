name := "implicit-collector"
version := "0.1"
scalaVersion := "2.12.3"
libraryDependencies ++= Seq(
    "org.scalameta" %% "testkit" % "2.0.0-M3",
    "com.github.scopt" % "scopt_2.12" % "3.7.0"
)

scalacOptions += "-Yno-adapted-args"
assemblyJarName in assembly := "implicit-collector.jar"