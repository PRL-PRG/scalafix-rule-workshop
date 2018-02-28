package cz.cvut.fit.prl.scalaimplicit.queries

import scala.language.reflectiveCalls

// TODO: do we want too keep helpers
// TODO: do we want to keep explict && combination using ','
trait SchemaMatchers {

  import Matchers._
  import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.Representation._

  def arguments[A <: {def arguments : Seq[ArgumentLike]}](x: Matcher[Seq[ArgumentLike]], xs: Matcher[Seq[ArgumentLike]]*): Matcher[A] =
    PropertyMatcher[A, Seq[ArgumentLike]]("arguments", _.arguments, x, xs)

  def arguments[A <: {def arguments : Seq[ArgumentLike]}] =
    new PropertyMatchHelper[A, Seq[ArgumentLike]]("arguments", _.arguments) with SeqMatchHelper[A, ArgumentLike]

  def code[A <: {def code : String}](x: Matcher[String], xs: Matcher[String]*): Matcher[A] =
    PropertyMatcher[A, String]("code", _.code, x, xs)

  def code[A <: {def code : String}] =
    new PropertyMatchHelper[A, String]("code", _.code) with StringMatchHelper[A]

  def col[A <: {def col : Int}](x: Matcher[Int], xs: Matcher[Int]*): Matcher[A] =
    PropertyMatcher[A, Int]("column", _.col, x, xs)

  def col[A <: {def col : Int}] =
    new PropertyMatchHelper[A, Int]("column", _.col) with OrderingMatchHelper[A, Int]

  def declaration[A <: {def declaration : Declaration}](x: Matcher[Declaration], xs: Matcher[Declaration]*): Matcher[A] =
    PropertyMatcher[A, Declaration]("declaration", _.declaration, x, xs)

  def file[A <: {def file : String}](x: Matcher[String], xs: Matcher[String]*): Matcher[A] =
    PropertyMatcher[A, String]("file", _.file, x, xs)

  def implicitArguments[A <: {def implicitArguments : Seq[ArgumentLike]}](x: Matcher[Seq[ArgumentLike]], xs: Matcher[Seq[ArgumentLike]]*): Matcher[A] =
    PropertyMatcher[A, Seq[ArgumentLike]]("implicitArguments", _.implicitArguments, x, xs)

  def implicitArguments[A <: {def implicitArguments : Seq[ArgumentLike]}] =
    new PropertyMatchHelper[A, Seq[ArgumentLike]]("implicitArguments", _.implicitArguments) with SeqMatchHelper[A, ArgumentLike]

  def isSynthetic[A <: {def isSynthetic : Boolean}]: Matcher[A] = PropertyMatcher[A]("synthetic", _.isSynthetic)

  def isImplicit[A <: {def isImplicit : Boolean}]: Matcher[A] = PropertyMatcher[A]("implicit", _.isImplicit)

  def file[A <: {def file : String}] =
    new PropertyMatchHelper[A, String]("file", _.file) with StringMatchHelper[A]

  def kind[A <: {def kind : String}](x: Matcher[String], xs: Matcher[String]*): Matcher[A] =
    PropertyMatcher[A, String]("kind", _.kind, x, xs)

  def kind[A <: {def kind : String}] =
    new PropertyMatchHelper[A, String]("kind", _.kind) with StringMatchHelper[A]

  def line[A <: {def line : Int}](x: Matcher[Int], xs: Matcher[Int]*): Matcher[A] =
    PropertyMatcher[A, Int]("line", _.line, x, xs)

  def line[A <: {def line : Int}] =
    new PropertyMatchHelper[A, Int]("line", _.line) with OrderingMatchHelper[A, Int]

  def location[A <: {def location : Option[Location]}](x: Matcher[Option[Location]], xs: Matcher[Option[Location]]*): Matcher[A] =
    PropertyMatcher[A, Option[Location]]("location", _.location, x, xs)

  def location[A <: {def location : Option[Location]}] =
    new PropertyMatchHelper[A, Option[Location]]("location", _.location) with OptionMatchHelper[A, Location]

  def name[A <: {def name : String}](x: Matcher[String], xs: Matcher[String]*): Matcher[A] =
    PropertyMatcher[A, String]("name", _.name, x, xs)

  def name[A <: {def name : String}] =
    new PropertyMatchHelper[A, String]("name", _.name) with StringMatchHelper[A]

  def parameters[A <: {def parameters : Seq[DeclaredParameter]}](x: Matcher[Seq[DeclaredParameter]], xs: Matcher[Seq[DeclaredParameter]]*): Matcher[A] =
    PropertyMatcher[A, Seq[DeclaredParameter]]("parameters", v => v.parameters, x, xs)

  def parameters[A <: {def parameters : Seq[Type]}] =
    new PropertyMatchHelper[A, Seq[Type]]("parameters", _.parameters) with SeqMatchHelper[A, Type]

  def parameterLists[A <: {def parameterLists : Seq[DeclaredParameterList]}](x: Matcher[Seq[DeclaredParameterList]], xs: Matcher[Seq[DeclaredParameterList]]*): Matcher[A] =
    PropertyMatcher[A, Seq[DeclaredParameterList]]("parameterLists", _.parameterLists, x, xs)

  def parameterLists[A <: {def parameterLists : Seq[DeclaredParameterList]}] =
    new PropertyMatchHelper[A, Seq[DeclaredParameterList]]("parameterLists", _.parameterLists) with SeqMatchHelper[A, DeclaredParameterList]

  def parents[A <: {def parents : Seq[Parent]}](x: Matcher[Seq[Parent]], xs: Matcher[Seq[Parent]]*): Matcher[A] =
    PropertyMatcher[A, Seq[Parent]]("parents", _.parents, x, xs)

  def parents[A <: {def parents : Seq[Parent]}] =
    new PropertyMatchHelper[A, Seq[Parent]]("parents", _.parents) with SeqMatchHelper[A, Parent]

  def returnType[A <: {def returnType : Option[Type]}](x: Matcher[Option[Type]], xs: Matcher[Option[Type]]*): Matcher[A] =
    PropertyMatcher[A, Option[Type]]("returnType", _.returnType, x, xs)

  def returnType[A <: {def returnType : Option[Type]}] =
    new PropertyMatchHelper[A, Option[Type]]("returnType", _.returnType) with OptionMatchHelper[A, Type]

  def signature[A <: {def signature : Option[Signature]}](x: Matcher[Option[Signature]], xs: Matcher[Option[Signature]]*): Matcher[A] =
    PropertyMatcher[A, Option[Signature]]("signature", _.signature, x, xs)

  def signature[A <: {def signature : Option[Signature]}] =
    new PropertyMatchHelper[A, Option[Signature]]("signature", _.signature) with OptionMatchHelper[A, Signature]

  def typeArguments[A <: {def typeArguments : Seq[Type]}](x: Matcher[Seq[Type]], xs: Matcher[Seq[Type]]*): Matcher[A] =
    PropertyMatcher[A, Seq[Type]]("typeArguments", _.typeArguments, x, xs)

  def typeArguments[A <: {def typeArguments : Seq[Type]}] =
    new PropertyMatchHelper[A, Seq[Type]]("typeArguments", _.typeArguments) with SeqMatchHelper[A, Type]

  def typeParameters[A <: {def typeParameters : Seq[Type]}](x: Matcher[Seq[Type]], xs: Matcher[Seq[Type]]*): Matcher[A] =
    PropertyMatcher[A, Seq[Type]]("typeParameters", _.typeParameters, x, xs)

  def typeParameters[A <: {def typeParameters : Seq[Type]}] =
    new PropertyMatchHelper[A, Seq[Type]]("typeParameters", _.typeParameters) with SeqMatchHelper[A, Type]
}

object SchemaMatchers extends SchemaMatchers