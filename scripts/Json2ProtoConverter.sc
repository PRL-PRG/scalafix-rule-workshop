import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, Path, Paths}
import java.util.stream.Collectors

import cz.cvut.fit.prl.scalaimplicit.core.util.Scala212Backport._
import cz.cvut.fit.prl.scalaimplicit.{schema => s}
import cz.cvut.fit.prl.scalaimplicit.schema.callSiteProtoCompanion
import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.{Representation => r}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.{JSONSerializer, ProtoSerializer}

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}
import io.circe.generic.auto._

val Root = "projects"
val InputFile = "results-callsites.json"
val OutputFile = "results-callsites.proto"

val paths =
  Files.find(Paths.get(Root), 3, (path: Path, _: BasicFileAttributes) => path.endsWith(InputFile))
    .collect(Collectors.toSet[Path])
    .asScala
    .toSeq

def convert(in: r.CallSite): s.CallSite = {
  def convertDeclaration(in: r.Declaration): s.Declaration =
    s.Declaration(
      name = in.name,
      kind = in.kind,
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
