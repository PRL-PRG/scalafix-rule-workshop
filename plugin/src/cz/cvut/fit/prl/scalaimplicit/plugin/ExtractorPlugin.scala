package cz.cvut.fit.prl.scalaimplicit.plugin

import cz.cvut.fit.prl.scalaimplicit.extractor.runners.SingleProjectWalker
import cz.cvut.fit.prl.scalaimplicit.extractor.{CSV, ExtractImplicits}
import sbt.Keys._
import sbt._
import sbt.plugins.JvmPlugin

object ExtractorPlugin extends AutoPlugin {
  override def requires = JvmPlugin
  val V: Map[(Long, Long), String] = Map(
    (2L -> 11L) -> "2.11.12",
    (2L -> 12L) -> "2.12.4"
  )
  def relevantProjects(state: State): Seq[(ProjectRef, String)] = {
    val extracted = Project.extract(state)
    for {
      p <- extracted.structure.allProjectRefs
      version <- scalaVersion.in(p).get(extracted.structure.data).toList
      partialVersion <- CrossVersion.partialVersion(version).toList
      fullVersion <- V.get(partialVersion).toList
    } yield p -> fullVersion
  }
  object autoImport {
    val reflect = inputKey[Unit]("Reflect on an fqn")
    val extract = taskKey[Unit]("Extract implicits")
  }
  import autoImport._
  import sbt.complete.Parsers.spaceDelimited
  override def trigger = allRequirements
  override lazy val projectSettings = inConfig(Compile)(Seq(
    reflect := {
      val classpath = (fullClasspath in Runtime).value
      val args: Seq[String] = spaceDelimited("<arg>").parsed
      Reflect(classpath, args(0), args(1))
    },
    extract := {
      Extract(".")
    }
  ))
}

object Reflect {
  def load(paths: Classpath): java.net.URLClassLoader = {
    new java.net.URLClassLoader(paths.map(_.data.toURI.toURL).toArray, this.getClass.getClassLoader)
  }
  def apply(classpath: Classpath, clas: String, thingName: String): Unit = {
    val loader = load(classpath)
    val ru = scala.reflect.runtime.universe
    val m = ru.runtimeMirror(loader)
    val cSymbol = m.staticClass(clas)
    val thing = cSymbol.typeSignature.members.find(_.name.toString == thingName).get
    println(s"Class ${cSymbol.toString} has thing ${thing}")
    println(s"  Signature: ${thing.typeSignature}")
  }
}

object Extract {
  def apply(root: String): Unit = {
    val walker = new SingleProjectWalker(root)
    val results = walker(ExtractImplicits)
    CSV.dumpFiles(root, results)
  }
}
