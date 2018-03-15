import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, Path, Paths}
import java.util.stream.Collectors

import $ivy.`cz.cvut.fit.prl::queries:0.1-SNAPSHOT`
import cz.cvut.fit.prl.scalaimplicit.core.util.Scala212Backport._
import cz.cvut.fit.prl.scalaimplicit.{schema => s}
import cz.cvut.fit.prl.scalaimplicit.schema.callSiteProtoCompanion
import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.{JSONSerializer, ProtoSerializer}

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}
import io.circe.generic.auto._


/**
  * Module to hold the internal representation of extracted information
  */
object Representation {

  case class Location(file: String, line: Int, col: Int) {
    override def toString: String = s"$file:$line:$col"
  }
  case class Type(name: String, parameters: Seq[Type] = Seq()) {
    def shortName =
      name
        .split("""\{""")
        .head
        .split("""\n""")
        .head
  }

  case class Declaration(name: String,
                         kind: String,
                         location: Option[Location],
                         isImplicit: Boolean,
                         signature: Option[Signature] = None,
                         parents: Seq[Parent] = Seq())
  case class Signature(typeParams: Seq[Type] = Seq(),
                       parameterLists: Seq[DeclaredParameterList] = Seq(),
                       returnType: Option[Type] = None)
  case class Parent(name: String,
                    declaration: Declaration,
                    typeArguments: Seq[Type])
  case class DeclaredParameterList(params: Seq[DeclaredParameter],
                                   isImplicit: Boolean)
  case class DeclaredParameter(name: String, tipe: Type)

  case class CallSite(name: String,
                      code: String,
                      location: Option[Location],
                      isSynthetic: Boolean,
                      declaration: Declaration,
                      typeArguments: Seq[Type],
                      implicitArguments: Seq[ArgumentLike])

  sealed trait ArgumentLike {
    def code: String
  }
  case class Argument(code: String) extends ArgumentLike
  case class ImplicitArgument(name: String,
                              code: String,
                              declaration: Declaration,
                              typeArguments: Seq[Type],
                              arguments: Seq[ArgumentLike])
    extends ArgumentLike
}


type Representation = r

class Job {
  val Root = "projects"
  val InputFile = "results-callsites.json"
  val OutputFile = "results-callsites.proto"

  val paths =
    Files.find(Paths.get(Root), 3, (path: Path, _: BasicFileAttributes) => path.endsWith(InputFile)).collect(Collectors.toSet[Path]).asScala.toSeq

  def convert(in: r.CallSite): s.CallSite = {
    def convertDeclaration(in: r.Declaration): s.Declaration =
      s.Declaration(
        name = in.name,
        kind = in.kind,
        location = in.location.map(convertLocation)
        isImplicit = in.isImplicit,
        signature = in.signature.map(convertSignature),
        parents = in.parents.map(convertParent)
      )

    def convertParent(in: r.Parent): s.Parent =
      s.Parent(
        name = in.name,
        declaration = convertDeclaration(in.declaration),
        typeArguments = in.typeArguments.map(convertType)
      )

    def convertSignature(in: r.Signature): s.Signature =
      s.Signature(
        typeParameters = in.typeParams.map(convertType),
        parameterLists = in.parameterLists.map(convertParameterList),
        returnType = in.returnType.map(convertType).get
      )

    def convertLocation(in: r.Location): s.Location =
      s.Location(file = in.file, line = in.line, col = in.col)

    def convertType(in: r.Type): s.Type =
      s.Type(name = in.name, parameters = in.parameters.map(convertType))

    def convertArgument(in: r.ArgumentLike): s.Argument = in match {
      case x: r.ImplicitArgument =>
        s.Argument(
          code = x.code,
          info = Some(
            s.ArgumentInfo(
              name = x.name,
              declaration = convertDeclaration(x.declaration),
              typeArguments = x.typeArguments.map(convertType),
              arguments = x.arguments.map(convertArgument)
            )
          )
        )
      case x: r.Argument =>
        s.Argument(code = x.code)
    }

    def convertParameterList(in: r.DeclaredParameterList): s.ParameterList =
      s.ParameterList(isImplicit = in.isImplicit, parameters = in.params.map(convertParameter))

    def convertParameter(in: r.DeclaredParameter): s.Parameter =
      s.Parameter(name = in.name, parameterType = convertType(in.tipe))

    s.CallSite(
      name = in.name,
      code = in.code,
      location = in.location.map(convertLocation),
      isSynthetic = in.isSynthetic,
      declaration = convertDeclaration(in.declaration),
      typeArguments = in.typeArguments.map(convertType),
      implicitArguments = in.implicitArguments.map(convertArgument)
    )
  }


  val start = System.currentTimeMillis()
  val res = paths
    .par
    .map { path =>
      val size = Files.size(path)
      println(s"Loading $path ($size bytes)")

      val protoFile = path.getParent.resolve(OutputFile)

      for {
        xs <- Try(JSONSerializer.loadJSON[List[r.CallSite]](path.toFile.getPath).map(convert))
        _ <- ProtoSerializer.save(xs, protoFile.toString)
      } yield size
    }
    .seq

  val end = System.currentTimeMillis()
  val elapsed = (end - start) / 1000
  val failures = res zip paths collect { case (Failure(e), path) => path -> e }
  val size = res.collect { case Success(s) => s }.sum

  failures.foreach { case (path, e) =>
    println("PATH: " + path)
    println(e)
    println()
  }

  println(s"** Finished (failed ${failures.size} / ${paths.size}) in $elapsed s ($size bytes ${size / elapsed} bytes/s)\n")
}

new Job()