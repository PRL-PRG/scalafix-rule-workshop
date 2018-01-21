package cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers
import cz.cvut.fit.prl.scalaimplicit.core.extractor.ExtractionResult
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.ProjectMetadata

import scalatags.Text.all.{script, _}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.Representation._
import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.HTMLSerializer.pprint
import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.PrettyPrinters.PrettyInstances.{
  PrettyArgument,
  PrettyCallSite,
  PrettyDeclaration
}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.PrettyPrinters._

object HTMLSerializer {
  trait HTMLPrintable[A] {
    def print(what: A)(implicit metadata: ProjectMetadata)
      : scalatags.Text.all.ConcreteHtmlTag[String]
  }

  def colitem(item: scalatags.Text.all.ConcreteHtmlTag[String]) =
    li(`class` := "collection-item")(item)

  def htable(style: String,
             rows: scalatags.Text.all.ConcreteHtmlTag[String]*) =
    table(`class` := style)(
      tbody(
        rows.map(x => tr(x))
      )
    )

  implicit object HCallSite extends HTMLPrintable[CallSite] {
    def print(what: CallSite)(implicit metadata: ProjectMetadata) = {
      li(
        div(`class` := "collapsible-header")(
          div(`class` := "container")(
            p(what.name),
            div(style := "align-content: right")(
              pprint(what.location)
            )
          )
        ),
        div(`class` := "collapsible-body")(
          htable(
            "bordered",
            tr(
              td(b("Definition")),
              td(pprint(what.declaration))
            ),
            tr(
              td(b("Implicit Arguments")),
              td(
                htable("bordered highlight",
                       what.implicitArguments.map(x => tr(td(pprint(x)))): _*)
              )
            )
          )
        )
      )
    }
  }
  implicit object HExtractionResult extends HTMLPrintable[ExtractionResult] {
    override def print(what: ExtractionResult)(
        implicit metadata: ProjectMetadata) = {
      div(`class` := "row")(
        div(`class` := "col s12")(
          ul(`class` := "collapsible popout",
             attr("data-collapsible") := "expandable")(
            what.callSites.map(
              x => pprint(x)
            )
          )
        )
      )
    }
  }
  implicit object HDefinition extends HTMLPrintable[Declaration] {
    override def print(what: Declaration)(implicit metadata: ProjectMetadata) =
      htable(
        "",
        tr(td(
             span(prettyPrint(what)(PrettyDeclaration).stripPrefix("""?:"""))
           ),
           td(pprint(what.location))))
  }
  implicit object HType extends HTMLPrintable[Type] {
    override def print(what: Type)(implicit metadata: ProjectMetadata) =
      ???
  }
  implicit object HParent extends HTMLPrintable[Parent] {
    override def print(what: Parent)(implicit metadata: ProjectMetadata) = ???
  }
  implicit object HSignature extends HTMLPrintable[Signature] {
    override def print(what: Signature)(implicit metadata: ProjectMetadata) =
      ???
  }
  implicit object HDeclaredParameterList
      extends HTMLPrintable[DeclaredParameterList] {
    override def print(what: DeclaredParameterList)(
        implicit metadata: ProjectMetadata) = ???
  }
  implicit object HDeclaredParameter extends HTMLPrintable[DeclaredParameter] {
    override def print(what: DeclaredParameter)(
        implicit metadata: ProjectMetadata) = ???
  }
  implicit object HArgumentLike extends HTMLPrintable[ArgumentLike] {
    override def print(what: ArgumentLike)(
        implicit metadata: ProjectMetadata) = {
      what match {
        case iarg: ImplicitArgument =>
          htable("",
                 tr(td(span("Name")), td(iarg.name)),
                 tr(td(span("Declaration")), td(pprint(iarg.declaration))))
        case arg: Argument => span(arg.code)
      }
    }
  }
  implicit object HLocation extends HTMLPrintable[Location] {

    def composeGHURL(what: Location, metadata: ProjectMetadata) =
      // Lines are 0-indexed on scalameta, but 1-indexed on Github, IntelliJ and everywhere else
      s"${metadata.url}/blob/${metadata.lastCommit}/${what.coords.get.file}#L${what.coords.get.line + 1}"

    override def print(what: Location)(implicit metadata: ProjectMetadata) = {
      what.coords match {
        case Some(coords) => {
          val url = composeGHURL(what, metadata)
          a(href := url,
            `class` := "secondary-content waves-effect waves-light btn-small",
            target := "_blank")(
            i(`class` := "fa fa-github", attr("aria-hidden") := "true")
          )
        }
        case None =>
          div(`class` := "secondary-content")(p("No location available"))
      }
    }
  }

  def pprint[A](x: A)(implicit printer: HTMLPrintable[A],
                      metadata: ProjectMetadata) = printer.print(x)

  def createDocument(x: ExtractionResult, metadata: ProjectMetadata): String = {
    html(
      head(
        link(
          rel := "stylesheet",
          href := "https://cdnjs.cloudflare.com/ajax/libs/materialize/0.100.2/css/materialize.min.css"),
        script(src := "https://use.fontawesome.com/7cd1eab442.js"),
        meta(name := "viewport",
             attr("content") := "width=device-width, initial-scale=1.0"),
        script(`type` := "text/javascript",
               src := "https://code.jquery.com/jquery-3.2.1.min.js"),
        script(
          src := "https://cdnjs.cloudflare.com/ajax/libs/materialize/0.100.2/js/materialize.min.js")
      ),
      body(
        // Header and all
        h1(s"Call Sites for ${metadata.name}"),
        h4(a(href := metadata.url)(metadata.url)),
        div(`class` := "container")(p(pprint(x)(HExtractionResult, metadata)))
      )
      // Footer
    ).render
  }
}
