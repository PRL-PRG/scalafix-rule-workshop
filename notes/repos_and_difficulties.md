1 - Change scalaVersion to 2.11.11
2 - Change scalaVersion to 2.12.3
3 - Add `scalacOptions ++= Seq("-Yrangepos")`
4 - Add `scalaVersion := "2.12.3"`

## Working

- ensime: NP
- finch: Change scalaVersion in build.sbt
- cats: NP

- FiloDB: 1
- securesocial:
    - 1 in `project/Common.scala`
    - 3 in every subproject's `.sbt` (`module-core/build.sbt`, `scalademo.sbt`...)
    - remove `module-core/test`, it doesn't compile
- fpinscala: 2
- lagom: Some dependency evictions and malformed inputs (MFI), but it works
- ammonite: Some MFI
- Kaman: 
    - 2 in all subprojects, done within `build.sbt`
    - 4 and 3 in all subprojects in `build.sbt`
    - Some MFIs

## Not working

- akka: Not working after checkout
- shapeless: 
    - 1 or 2
    - Weird error in coreJVM: "Enclosing tree does not incluide tree"
- sbteclipse: Cannot compile macros from from the original version, which is incompatible with scalafix
- algebird: Their git structure is weird. We cloned the develop branch, and not master. Hard to switch to master

