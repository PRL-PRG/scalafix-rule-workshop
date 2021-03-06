import com.trueaccord.scalapb.compiler.Version.scalapbVersion

lazy val fs2Version = "0.10.1"
lazy val circeVersion = "0.9.0"

lazy val commonSettings = Seq(
  test in assembly := {},
  scalaVersion := "2.11.11",
  crossScalaVersions := Vector("2.12.4", "2.11.11"),
  sbtVersion in Global := "1.0.4",
  crossSbtVersions := Vector("0.13.16", "1.0.4"),
  scalacOptions += "-Yno-adapted-args",
  organization := "cz.cvut.fit.prl",
  scalaCompilerBridgeSource := {
    val sv = appConfiguration.value.provider.id.version
    ("org.scala-sbt" % "compiler-interface" % sv % "component").sources
  }
)

lazy val root = (project in file("."))
  .aggregate(coreutils, queries, implicitExtractor, callSiteCounter)
  .settings(commonSettings: _*)

lazy val coreutils = (project in file("core"))
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "ch.qos.logback" % "logback-classic" % "1.2.3",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
      "org.scalameta" %% "testkit" % "2.1.2",
      //"org.scalameta" % "semanticdb-scalac_2.12.4" % "2.1.2",
      "org.scalameta" % "semanticdb-scalac_2.11.11" % "2.1.2",
      "org.scalameta" % "semanticdb_2.11" % "2.1.2",
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "com.github.scopt" % "scopt_2.11" % "3.7.0",
      //"com.github.scopt" % "scopt_2.12" % "3.7.0",
      "org.scalactic" %% "scalactic" % "3.0.4",
      "org.scalatest" %% "scalatest" % "3.0.4" % "test",
      "com.github.nikita-volkov" % "sext" % "0.2.4",
      "io.suzaku" %% "boopickle" % "1.2.6",
      "org.json4s" %% "json4s-native" % "3.6.0-M2",
      "com.lihaoyi" %% "scalatags" % "0.6.7",
      "com.github.tototoshi" %% "scala-csv" % "1.3.5",
      "org.spire-math" %% "jawn-json4s" % "0.11.0",
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,
      "com.trueaccord.scalapb" %% "scalapb-runtime" % scalapbVersion
    ),
    PB.targets in Compile := Seq(
      scalapb.gen() -> (sourceManaged in Compile).value
    )
  )

lazy val queries = (project in file("queries"))
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "scalatags" % "0.6.7"
    ))
  .dependsOn(coreutils % "test->test", coreutils % "compile->test")

lazy val implicitExtractor = (project in file("extractor"))
  .settings(commonSettings: _*)
  .settings(
    assemblyJarName in assembly := "implicit-analyzer.jar",
    assemblyMergeStrategy in assembly := {
      case PathList("META-INF", xs@_*) => MergeStrategy.discard
      case x => MergeStrategy.first
    }
  )
  .dependsOn(coreutils)

lazy val callSiteCounter = (project in file("cs-counter"))
  .settings(commonSettings: _*)
  .settings(
    assemblyJarName in assembly := "callsite-counter.jar",
    libraryDependencies ++= Seq(
      "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
      //"com.github.scopt" % "scopt_2.12" % "3.7.0"
      "com.github.scopt" %% "scopt" % "3.7.0"
    ),
    assemblyMergeStrategy in assembly := {
      case PathList("META-INF", xs@_*) => MergeStrategy.discard
      case x => MergeStrategy.first
    }
  )
  .dependsOn(coreutils % "test->test", coreutils)

lazy val jsonReencoder = (project in file("json-reencoder"))
  .settings(commonSettings: _*)
  .settings(
    assemblyJarName in assembly := "reencoder.jar",
    libraryDependencies ++= Seq(
      "com.github.scopt" %% "scopt" % "3.7.0",
      "org.json4s" %% "json4s-native" % "3.6.0-M2",
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion
    )
  )
  .dependsOn(coreutils)
