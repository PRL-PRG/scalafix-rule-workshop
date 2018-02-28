package cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers

import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.Representation

object PrettyPrinters {
  import Representation._
  trait PrettyPrintable[T] {
    def pretty(t: T, indent: Int): String
  }

  object PrettyInstances {

    implicit object PrettyLocation extends PrettyPrintable[Option[Location]] {
      override def pretty(t: Option[Location], indent: Int): String = {
        t match {
          case Some(loc) => s"[${loc.toString}]:"
          case None => "?:"
        }
      }
    }

    implicit object PrettyType extends PrettyPrintable[Type] {
      override def pretty(t: Type, indent: Int): String = {
        prettyPrint(t.typeParameters, indent + 2) match {
          case p if p == "" => s"${t.name}"
          case p => s"${t.name}[$p]"
        }
      }
    }

    implicit object PrettyTypeOption extends PrettyPrintable[Option[Type]] {
      override def pretty(t: Option[Type], indent: Int): String = {
        t match {
          case Some(tipe) => prettyPrint(tipe)
          case None => ""
        }
      }
    }

    implicit object PrettyDeclaredParameter
        extends PrettyPrintable[DeclaredParameter] {
      override def pretty(t: DeclaredParameter, indent: Int): String = {
        s"${t.name}: ${prettyPrint(t.parameterType, indent + 2)}"
      }
    }

    implicit object PrettyDeclaredParameterList
        extends PrettyPrintable[DeclaredParameterList] {
      override def pretty(t: DeclaredParameterList, indent: Int): String = {
        val prefix = if (t.isImplicit) s"implicit " else ""
        s"(${prefix}${prettyPrint(t.parameters, indent + 2)})"
      }
    }

    implicit object PrettySignature
        extends PrettyPrintable[Option[Signature]] {
      override def pretty(sign: Option[Signature], indent: Int): String = {
        sign match {
          case Some(sign) => {
            val typeParams = wrapIfSome(prettyPrint(sign.typeParameters), "[", "]")
            val parameterLists = prettyPrint(sign.parameterLists)
            val retType = wrapIfSome(prettyPrint(sign.returnType), ": ")
            s"$typeParams$parameterLists$retType"
          }
          case None => ""
        }
      }
    }

    implicit object PrettyParent extends PrettyPrintable[Parent] {
      def getMatchedTypes(tparams: Seq[Type], targs: Seq[Type]): String = {
        assert(tparams.size == targs.size,
               "Different number of type parameters and arguments")
        (tparams zip targs)
          .map(pair => s"${prettyPrint(pair._1)} = ${prettyPrint(pair._2)}")
          .mkString(",")
      }

      override def pretty(t: Parent, indent: Int): String = {
        val tparams = t.declaration.signature.get.typeParameters
        val targs =
          wrapIfSome(getMatchedTypes(tparams, t.typeArguments), "[", "]")
        val kind =
          if (t.declaration.isImplicit) s"implicit ${t.declaration.kind}"
          else t.declaration.kind
        s"$kind ${t.name}$targs"
      }
    }

    implicit object PrettyDeclaration extends PrettyPrintable[Declaration] {
      override def pretty(t: Declaration, indent: Int): String = {
        val realKind = if (t.isImplicit) s"implicit ${t.kind}" else t.kind
        val signature = prettyPrint(t.signature, indent + 2)
        val parents =
          wrapIfSome(prettyPrint(t.parents, indent + 2), " extends (", ")")
        s"${prettyPrint(t.location)}${" " * indent}$realKind ${t.name}"
          .concat(signature)
          .concat(parents)
      }
    }

    implicit object PrettyArgument extends PrettyPrintable[ArgumentLike] {
      override def pretty(arg: ArgumentLike, indent: Int): String = {
        arg match {
          case t: Argument => {
            s"""${" " * indent}arg: ${t.code}"""
          }
          case t: ImplicitArgument => {
            s"""${" " * indent}iarg: ${t.name}${wrapIfSome(
                 prettyPrint(t.typeArguments, indent + 2),
                 "[",
                 "]")}
               |${prettyPrint(t.declaration, indent + 2)}
               |${prettyPrint(t.arguments, indent + 2)}""".stripMargin
          }
        }
      }
    }

    implicit object PrettyCallSite extends PrettyPrintable[CallSite] {
      def pretty(cs: CallSite, indent: Int): String = {
        val prefix = if (cs.isSynthetic) "s" else ""
        s"""${prettyPrint(cs.location)}${" " * indent}${prefix}cs: ${cs.name}${wrapIfSome(
             prettyPrint(cs.typeArguments, indent + 2),
             "[",
             "]")}
           |${prettyPrint(cs.declaration, indent + 2)}
           |${cs.implicitArguments
             .map(PrettyArgument.pretty(_, indent + 2))
             .mkString("\n")}""".stripMargin
      }
    }

    implicit val separator: String = ", "
    implicit def PrettySeq[T](
        implicit separator: String,
        printer: PrettyPrintable[T]): PrettyPrintable[Seq[T]] =
      new PrettyPrintable[Seq[T]] {
        override def pretty(seq: Seq[T], indent: Int): String =
          if (seq.nonEmpty)
            seq
              .map(elem => s"${printer.pretty(elem, indent)}")
              .mkString(separator)
          else ""
      }

    def wrapIfSome(content: String, begin: String = "", end: String = "") =
      if (content.nonEmpty) begin + content + end
      else ""
  }

  def prettyPrint[T](some: T, startIndent: Int = 0)(
      implicit printer: PrettyPrintable[T]): String = {
    val res = printer.pretty(some, startIndent)
    res
  }
}
