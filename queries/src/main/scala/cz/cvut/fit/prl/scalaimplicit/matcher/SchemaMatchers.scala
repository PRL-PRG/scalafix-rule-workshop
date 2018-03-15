package cz.cvut.fit.prl.scalaimplicit.matcher

import cz.cvut.fit.prl.scalaimplicit.schema._
import cz.cvut.fit.prl.scalaimplicit.matcher.OverloadHacks._
import cz.cvut.fit.prl.scalaimplicit.matcher.LogicalMatchers._

import scala.language.reflectiveCalls

trait SchemaMatchers {
  def arguments[A <: {def arguments : Seq[Argument]}](x: Matcher[Seq[Argument]], xs: Matcher[Seq[Argument]]*): Matcher[A] =
    PropertyMatcher("arguments", _.arguments, combineAnd(x +: xs))

  def code[A <: {def code : String}](x: Matcher[String], xs: Matcher[String]*): Matcher[A] =
    PropertyMatcher("code", _.code, combineAnd(x +: xs))

  def col[A <: {def col : Int}](x: Matcher[Int], xs: Matcher[Int]*): Matcher[A] =
    PropertyMatcher("column", _.col, combineAnd(x +: xs))

  def declaration[A <: {def declaration : Declaration}](x: Matcher[Declaration], xs: Matcher[Declaration]*): Matcher[A] =
    PropertyMatcher("declaration", _.declaration, combineAnd(x +: xs))

  def file[A <: {def file : String}](x: Matcher[String], xs: Matcher[String]*): Matcher[A] =
    PropertyMatcher("file", _.file, combineAnd(x +: xs))

  def implicitArguments[A <: {def implicitArguments : Seq[Argument]}](x: Matcher[Seq[Argument]], xs: Matcher[Seq[Argument]]*): Matcher[A] =
    PropertyMatcher("implicitArguments", _.implicitArguments, combineAnd(x +: xs))

  def isImplicit[A <: {def isImplicit : Boolean}]: Matcher[A] = BooleanPropertyMatcher("implicit", _.isImplicit)

  def isSynthetic[A <: {def isSynthetic : Boolean}]: Matcher[A] = BooleanPropertyMatcher("synthetic", _.isSynthetic)

  def kind[A <: {def kind : String}](x: Matcher[String], xs: Matcher[String]*): Matcher[A] =
    PropertyMatcher("kind", _.kind, combineAnd(x +: xs))

  def line[A <: {def line : Int}](x: Matcher[Int], xs: Matcher[Int]*): Matcher[A] =
    PropertyMatcher("line", _.line, combineAnd(x +: xs))

  def location[A <: {def location : Option[Location]}](x: Matcher[Option[Location]], xs: Matcher[Option[Location]]*)(implicit o: OverloadHack1): Matcher[A] =
    PropertyMatcher("location", _.location, combineAnd(x +: xs))

  def location[A <: {def location : Option[Location]}](x: Matcher[Location], xs: Matcher[Location]*)(implicit o: OverloadHack2): Matcher[A] =
    OptionPropertyMatcher("location", _.location, combineAnd(x +: xs))

  def name[A <: {def name : String}](x: Matcher[String], xs: Matcher[String]*): Matcher[A] =
    PropertyMatcher("name", _.name, combineAnd(x +: xs))

  def parameters[A <: {def parameters : Seq[Parameter]}](x: Matcher[Seq[Parameter]], xs: Matcher[Seq[Parameter]]*): Matcher[A] =
    PropertyMatcher("parameters", v => v.parameters, combineAnd(x +: xs))

  def parameterLists[A <: {def parameterLists : Seq[ParameterList]}](x: Matcher[Seq[ParameterList]], xs: Matcher[Seq[ParameterList]]*): Matcher[A] =
    PropertyMatcher("parameterLists", _.parameterLists, combineAnd(x +: xs))

  def parents[A <: {def parents : Seq[Parent]}](x: Matcher[Seq[Parent]], xs: Matcher[Seq[Parent]]*): Matcher[A] =
    PropertyMatcher("parents", _.parents, combineAnd(x +: xs))

  def returnType[A <: {def returnType : Option[Type]}](x: Matcher[Option[Type]], xs: Matcher[Option[Type]]*)(implicit o: OverloadHack1): Matcher[A] =
    PropertyMatcher("returnType", _.returnType, combineAnd(x +: xs))

  def returnType[A <: {def returnType : Option[Type]}](x: Matcher[Type], xs: Matcher[Type]*)(implicit o: OverloadHack2): Matcher[A] =
    OptionPropertyMatcher("returnType", _.returnType, combineAnd(x +: xs))

  def signature[A <: {def signature : Option[Signature]}](x: Matcher[Option[Signature]], xs: Matcher[Option[Signature]]*)(implicit o: OverloadHack1): Matcher[A] =
    PropertyMatcher("signature", _.signature, combineAnd(x +: xs))

  def signature[A <: {def signature : Option[Signature]}](x: Matcher[Signature], xs: Matcher[Signature]*)(implicit o: OverloadHack2): Matcher[A] =
    OptionPropertyMatcher("signature", _.signature, combineAnd(x +: xs))

  def typeArguments[A <: {def typeArguments : Seq[Type]}](x: Matcher[Seq[Type]], xs: Matcher[Seq[Type]]*): Matcher[A] =
    PropertyMatcher("typeArguments", _.typeArguments, combineAnd(x +: xs))

  def typeParameters[A <: {def typeParameters : Seq[Type]}](x: Matcher[Seq[Type]], xs: Matcher[Seq[Type]]*): Matcher[A] =
    PropertyMatcher("typeParameters", _.typeParameters, combineAnd(x +: xs))
}

object SchemaMatchers extends SchemaMatchers