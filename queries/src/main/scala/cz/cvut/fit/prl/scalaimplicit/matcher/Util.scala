package cz.cvut.fit.prl.scalaimplicit.matcher

import reflect.runtime.universe._
import reflect.runtime.currentMirror

object Util {

  implicit class PrettyToString[A](that: A) {
    def toPrettyString: String = {

      def indent(s: String) = s.lines.toStream match {
        case h +: t =>
          (("- " + h) +: t.map("| " + _)) mkString "\n"
        case _ => "- "
      }

      def typeName(x: Any) = x match {
        case x: List[_] => s": List(${x.size})"
        case x: Product => ": " + x.productPrefix
        case _ => ": " + x.getClass.getSimpleName
      }

      def fmt(x: Any) = x match {
        case x: CharSequence => "\"" + x + "\""
        case _ => x
      }

      def prettify(a: Any): String = {
        a match {
          case (k, v) =>
            prettify(k) + "\n" + prettify(v)
          case a: TraversableOnce[_] =>
            a.toStream
              .map(prettify)
              .map(indent)
              .mkString("\n")
          case a: Product =>
            val fields = currentMirror.reflect(a).symbol.typeSignature.members.toStream
              .collect { case a: TermSymbol => a }
              .filterNot(_.isMethod)
              .filterNot(_.isModule)
              .filterNot(_.isClass)
              .map(currentMirror.reflect(a).reflectField)
              .map(f => (f.symbol.name.toString.trim + typeName(f.get)) -> fmt(f.get))
              .reverse
            prettify(fields)
          case null =>
            "null"
          case _ =>
            a.toString
        }
      }

      typeName(that) + "\n" + prettify(that)
    }
  }
}