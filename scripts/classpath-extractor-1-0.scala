package cz.cvut.fit.prl.scalaimplicit

import java.io._

import sbt._
import sbt.Keys._
import sbt.plugins.JvmPlugin
import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets

object ClassPathExtractorPlugin extends AutoPlugin {
  override def requires: Plugins = JvmPlugin
  object autoImport {
    val gcp = taskKey[Unit]("Print out the classpath")
  }
  import autoImport._
  override def trigger: PluginTrigger = allRequirements
  override lazy val projectSettings = inConfig(Compile)(
    Seq(
      gcp := {
        val csp = (fullClasspath in Runtime).value.files
        val tcsp = (fullClasspath in Test).value.files
        PrintClasspath(csp ++ tcsp, "./classpath.dat")
      }
    ))
}

object PrintClasspath {
  def apply(csps: Seq[File], target: String) = {
    import java.nio._
    import java.nio.file._
    import java.nio.charset._
    val contents = csps
      .map(_.getAbsolutePath)
      .toSet
      .mkString("\n")
      .getBytes(StandardCharsets.US_ASCII)
    if (Files.exists(Paths.get(target)))
      Files.write(Paths.get(target), contents, StandardOpenOption.APPEND)
    else Files.write(Paths.get(target), contents)
  }
}
