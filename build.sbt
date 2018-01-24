lazy val commonSettings = Seq(
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
  .aggregate(coreutils, macros, queries, implicitExtractor)
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
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "com.github.scopt" % "scopt_2.11" % "3.7.0",
      //"com.github.scopt" % "scopt_2.12" % "3.7.0",
      "org.scalactic" %% "scalactic" % "3.0.4",
      "org.scalatest" %% "scalatest" % "3.0.4" % "test",
      "com.github.nikita-volkov" % "sext" % "0.2.4",
      "io.suzaku" %% "boopickle" % "1.2.6",
      "org.json4s" %% "json4s-native" % "3.6.0-M2",
      "com.lihaoyi" %% "scalatags" % "0.6.7"
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
  .settings(libraryDependencies ++= Seq(
    "com.lihaoyi" %% "scalatags" % "0.6.7",
    "org.apache.spark" %% "spark-core" % "2.1.0" % "provided",
    "org.apache.spark" %% "spark-sql" % "2.2.1",
    "org.apache.spark" %% "spark-catalyst" % "2.2.1"
  ))
  .dependsOn(coreutils % "test->test", macros)

lazy val classpathExtractor =
  (project in file("sbt-classpath-extractor"))
    .settings(
      sbtVersion in Global := "0.13.16",
      crossSbtVersions := Vector("0.13.16", "1.0.4"),
      sbtPlugin := true,
      scalaCompilerBridgeSource := {
        val sv = appConfiguration.value.provider.id.version
        ("org.scala-sbt" % "compiler-interface" % sv % "component").sources
      },
      organization := "cz.cvut.fit.prl",
      name := "sbt-classpath-extractor",
      version := "0.4-SNAPSHOT"
    )

lazy val implicitExtractor = (project in file("application"))
  .settings(commonSettings: _*)
  .settings(
    assemblyJarName in assembly := "implicit-collector.jar"
  )
  .dependsOn(coreutils)
