package cz.cvut.fit.prl.scalaimplicit.queries

import scala.language.reflectiveCalls

// TODO: do we want too keep helpers
// TODO: do we want to keep explict && combination using ','
trait SchemaMatchers {

  import Matchers._
  import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.Representation._


  //  def arguments[A <: {def arguments : Seq[ArgumentLike]}](x: Matcher[Seq[ArgumentLike]], xs: Matcher[Seq[ArgumentLike]]*): Matcher[A] =
  //    PropertyMatcher[A, Seq[ArgumentLike]]("arguments", _.arguments, x, xs)
  //
  //  def arguments[A <: {def arguments : Seq[ArgumentLike]}] =
  //    new PropertyMatchHelper[A, Seq[ArgumentLike]]("arguments", _.arguments) with SeqMatchHelper[A, ArgumentLike]
  //
  //  def code[A <: {def code : String}](x: Matcher[String], xs: Matcher[String]*): Matcher[A] =
  //    PropertyMatcher[A, String]("code", _.code, x, xs)
  //
  //  def code[A <: {def code : String}] =
  //    new PropertyMatchHelper[A, String]("code", _.code) with StringMatchHelper[A]
  //
  //  def col[A <: {def col : Int}](x: Matcher[Int], xs: Matcher[Int]*): Matcher[A] =
  //    PropertyMatcher[A, Int]("column", _.col, x, xs)
  //
  //  def col[A <: {def col : Int}] =
  //    new PropertyMatchHelper[A, Int]("column", _.col) with OrderingMatchHelper[A, Int]
  //
  //  def declaration[A <: {def declaration : Declaration}](x: Matcher[Declaration], xs: Matcher[Declaration]*): Matcher[A] =
  //    PropertyMatcher[A, Declaration]("declaration", _.declaration, x, xs)
  //
  //  def file[A <: {def file : String}](x: Matcher[String], xs: Matcher[String]*): Matcher[A] =
  //    PropertyMatcher[A, String]("file", _.file, x, xs)
  //
  //  def implicitArguments[A <: {def implicitArguments : Seq[ArgumentLike]}](x: Matcher[Seq[ArgumentLike]], xs: Matcher[Seq[ArgumentLike]]*): Matcher[A] =
  //    PropertyMatcher[A, Seq[ArgumentLike]]("implicitArguments", _.implicitArguments, x, xs)
  //
  //  def implicitArguments[A <: {def implicitArguments : Seq[ArgumentLike]}] =
  //    new PropertyMatchHelper[A, Seq[ArgumentLike]]("implicitArguments", _.implicitArguments) with SeqMatchHelper[A, ArgumentLike]
  //
  //

  trait PDeclaration

  trait PKind

  trait PIsImplicit

  trait PIsSynthetic

  trait PName

  trait PParameterLists

  trait PParameters

  trait PSignature

  implicit def PGDeclaration[A <: {def declaration : Declaration}]: PG[A, PDeclaration, Declaration] = PG(_.declaration)

  implicit def PGIsImplicit[A <: {def isImplicit : Boolean}]: PG[A, PIsImplicit, Boolean] = PG(_.isImplicit)

  implicit def PGIsSynthetic[A <: {def isSynthetic : Boolean}]: PG[A, PIsSynthetic, Boolean] = PG(_.isSynthetic)

  implicit def PGKind[A <: {def kind : String}]: PG[A, PKind, String] = PG(_.kind)

  implicit def PGName[A <: {def name : String}]: PG[A, PName, String] = PG(_.name)

  implicit def PGParameterLists[A <: {def parameterLists : Seq[DeclaredParameterList]}]: PG[A, PParameterLists, Seq[DeclaredParameterList]] = PG(_.parameterLists)

  implicit def PGParameters[A <: {def params : Seq[DeclaredParameter]}]: PG[A, PParameters, Seq[DeclaredParameter]] = PG(_.params)

  implicit def PGSignature[A <: {def signature : Option[Signature]}]: PG[A, PSignature, Option[Signature]] = PG(_.signature)

  def declaration[A](x: Matcher[Declaration])(implicit pg: PG[A, PDeclaration, Declaration]): Matcher[A] = PropertyMatcher("declaration", x)

  def isImplicit[A](implicit pg: PG[A, PIsImplicit, Boolean]): Matcher[A] = BooleanPropertyMatcher("implicit")(pg)

  def isSynthetic[A](implicit pg: PG[A, PIsSynthetic, Boolean]): Matcher[A] = BooleanPropertyMatcher("synthetic")(pg)

  def kind[A](x: Matcher[String])(implicit pg: PG[A, PKind, String]): Matcher[A] = PropertyMatcher("kind", x)

  def name[A](x: Matcher[String])(implicit pg: PG[A, PName, String]): Matcher[A] = PropertyMatcher("name", x)

  def parameterLists[A](x: Matcher[Seq[DeclaredParameterList]])(implicit pg: PG[A, PParameterLists, Seq[DeclaredParameterList]]): Matcher[A] = PropertyMatcher("parameter list", x)

  def parameters[A](x: Matcher[Seq[DeclaredParameter]])(implicit pg: PG[A, PParameters, Seq[DeclaredParameter]]): Matcher[A] = PropertyMatcher("parameters", x)

  trait OverloadHack1

  trait OverloadHack2

  implicit val overloadHack1 = new OverloadHack1 {}
  implicit val overloadHack2 = new OverloadHack2 {}

  def signature[A](x: Matcher[Option[Signature]])(implicit pg: PG[A, PSignature, Option[Signature]], ov: OverloadHack1): Matcher[A] = PropertyMatcher("signature", x)

  def signature[A](x: Matcher[Signature])(implicit pg: PG[A, PSignature, Option[Signature]], ov: OverloadHack2): Matcher[A] = new OptionMatcher(pg.get, x)

  class OptionMatcher[A, B](f: A => Option[B], x: Matcher[B]) extends Matcher[A] {
    override def test(v: A): Boolean = f(v).exists(x.test)

    override def description: String = ???

    override def negativeDescription: String = ???

    override def describeMatch(v: A): Option[String] = ???

    override def describeMismatch(v: A): Option[String] = ???
  }


  //  def isImplicit[A <: {def isImplicit : Boolean}]: Matcher[A] = PropertyMatcher[A]("implicit", _.isImplicit)
  //
  //  def file[A <: {def file : String}] =
  //    new PropertyMatchHelper[A, String]("file", _.file) with StringMatchHelper[A]
  //
  //  def kind[A <: {def kind : String}](x: Matcher[String], xs: Matcher[String]*): Matcher[A] =
  //    PropertyMatcher[A, String]("kind", _.kind, x, xs)
  //
  //  def kind[A <: {def kind : String}] =
  //    new PropertyMatchHelper[A, String]("kind", _.kind) with StringMatchHelper[A]
  //
  //  def line[A <: {def line : Int}](x: Matcher[Int], xs: Matcher[Int]*): Matcher[A] =
  //    PropertyMatcher[A, Int]("line", _.line, x, xs)
  //
  //  def line[A <: {def line : Int}] =
  //    new PropertyMatchHelper[A, Int]("line", _.line) with OrderingMatchHelper[A, Int]
  //
  //  def location[A <: {def location : Option[Location]}](x: Matcher[Option[Location]], xs: Matcher[Option[Location]]*): Matcher[A] =
  //    PropertyMatcher[A, Option[Location]]("location", _.location, x, xs)
  //
  //  def location[A <: {def location : Option[Location]}] =
  //    new PropertyMatchHelper[A, Option[Location]]("location", _.location) with OptionMatchHelper[A, Location]
  //
  //  def name[A <: {def name : String}](x: Matcher[String], xs: Matcher[String]*): Matcher[A] =
  //    PropertyMatcher[A, String]("name", _.name, x, xs)
  //
  //  def name[A <: {def name : String}] =
  //    new PropertyMatchHelper[A, String]("name", _.name) with StringMatchHelper[A]
  //
  //  def parameters[A <: {def parameters : Seq[DeclaredParameter]}](x: Matcher[Seq[DeclaredParameter]], xs: Matcher[Seq[DeclaredParameter]]*): Matcher[A] =
  //    PropertyMatcher[A, Seq[DeclaredParameter]]("parameters", v => v.parameters, x, xs)
  //
  //  def parameters[A <: {def parameters : Seq[Type]}] =
  //    new PropertyMatchHelper[A, Seq[Type]]("parameters", _.parameters) with SeqMatchHelper[A, Type]
  //
  //  def parameterLists[A <: {def parameterLists : Seq[DeclaredParameterList]}](x: Matcher[Seq[DeclaredParameterList]], xs: Matcher[Seq[DeclaredParameterList]]*): Matcher[A] =
  //    PropertyMatcher[A, Seq[DeclaredParameterList]]("parameterLists", _.parameterLists, x, xs)
  //
  //  def parameterLists[A <: {def parameterLists : Seq[DeclaredParameterList]}] =
  //    new PropertyMatchHelper[A, Seq[DeclaredParameterList]]("parameterLists", _.parameterLists) with SeqMatchHelper[A, DeclaredParameterList]
  //
  //  def parents[A <: {def parents : Seq[Parent]}](x: Matcher[Seq[Parent]], xs: Matcher[Seq[Parent]]*): Matcher[A] =
  //    PropertyMatcher[A, Seq[Parent]]("parents", _.parents, x, xs)
  //
  //  def parents[A <: {def parents : Seq[Parent]}] =
  //    new PropertyMatchHelper[A, Seq[Parent]]("parents", _.parents) with SeqMatchHelper[A, Parent]
  //
  //  def returnType[A <: {def returnType : Option[Type]}](x: Matcher[Option[Type]], xs: Matcher[Option[Type]]*): Matcher[A] =
  //    PropertyMatcher[A, Option[Type]]("returnType", _.returnType, x, xs)
  //
  //  def returnType[A <: {def returnType : Option[Type]}] =
  //    new PropertyMatchHelper[A, Option[Type]]("returnType", _.returnType) with OptionMatchHelper[A, Type]
  //
  //  def signature[A <: {def signature : Option[Signature]}](x: Matcher[Option[Signature]], xs: Matcher[Option[Signature]]*): Matcher[A] =
  //    PropertyMatcher[A, Option[Signature]]("signature", _.signature, x, xs)
  //
  //  def signature[A <: {def signature : Option[Signature]}] =
  //    new PropertyMatchHelper[A, Option[Signature]]("signature", _.signature) with OptionMatchHelper[A, Signature]
  //
  //  def typeArguments[A <: {def typeArguments : Seq[Type]}](x: Matcher[Seq[Type]], xs: Matcher[Seq[Type]]*): Matcher[A] =
  //    PropertyMatcher[A, Seq[Type]]("typeArguments", _.typeArguments, x, xs)
  //
  //  def typeArguments[A <: {def typeArguments : Seq[Type]}] =
  //    new PropertyMatchHelper[A, Seq[Type]]("typeArguments", _.typeArguments) with SeqMatchHelper[A, Type]
  //
  //  def typeParameters[A <: {def typeParameters : Seq[Type]}](x: Matcher[Seq[Type]], xs: Matcher[Seq[Type]]*): Matcher[A] =
  //    PropertyMatcher[A, Seq[Type]]("typeParameters", _.typeParameters, x, xs)
  //
  //  def typeParameters[A <: {def typeParameters : Seq[Type]}] =
  //    new PropertyMatchHelper[A, Seq[Type]]("typeParameters", _.typeParameters) with SeqMatchHelper[A, Type]
}

object SchemaMatchers extends SchemaMatchers