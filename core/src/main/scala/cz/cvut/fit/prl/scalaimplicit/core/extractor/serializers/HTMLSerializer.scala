package cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers
import cz.cvut.fit.prl.scalaimplicit.core.extractor.ImplicitAnalysisResult
import cz.cvut.fit.prl.scalaimplicit.schema._
import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.PrettyPrinters.PrettyInstances.{
  PrettyDeclaration,
  PrettyLocation
}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.PrettyPrinters._
import cz.cvut.fit.prl.scalaimplicit.core.reports._

import scalatags.Text.all.{script, _}

object HTMLSerializer {
  type HTMLTag = scalatags.Text.all.ConcreteHtmlTag[String]

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
  implicit object HExtractionResult
      extends HTMLPrintable[ImplicitAnalysisResult] {
    override def print(what: ImplicitAnalysisResult)(
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
  implicit object HParameterList
      extends HTMLPrintable[ParameterList] {
    override def print(what: ParameterList)(
        implicit metadata: ProjectMetadata) = ???
  }
  implicit object HParameter extends HTMLPrintable[Parameter] {
    override def print(what: Parameter)(
        implicit metadata: ProjectMetadata) = ???
  }
  implicit object HArgumentLike extends HTMLPrintable[Argument] {
    override def print(what: Argument)(
        implicit metadata: ProjectMetadata) = {
      what.info match {
        case Some(info) =>
          htable("",
                 tr(td(span("Name")), td(info.name)),
                 tr(td(span("Declaration")), td(pprint(info.declaration))))
        case None => span(what.code)
      }
    }
  }
  implicit object HLocation extends HTMLPrintable[Option[Location]] {

    def composeGHURL(what: Location, metadata: ProjectMetadata) =
      // Lines are 0-indexed on scalameta, but 1-indexed on Github, IntelliJ and everywhere else
      s"${metadata.url}/blob/${metadata.lastCommit}/${what.file}#L${what.line + 1}"

    def composeGistItURL(what: Location, metadata: ProjectMetadata) =
      // Lines are 0-indexed on scalameta, but 1-indexed on Github, IntelliJ and everywhere else
      s"http://gist-it.appspot.com/${metadata.url}/blob/${metadata.lastCommit}/${what.file}?slice=${what.line + 1 - 5}:${what.line + 1 + 5}"

    override def print(what: Option[Location])(
        implicit metadata: ProjectMetadata) = {
      what match {
        case Some(loc) => {
          val url = composeGHURL(loc, metadata)
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

  def createDocument(results: Seq[ProjectReport]): String = {
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
        div(`class` := "")(
          div(`class` := "row")(
            // Header and all
            div(`class` := "col s2 teal lighten-5")(
              b("Projects"),
              ul(id := "navbar")(
                results.map(res =>
                  li(a(href := s"#${res.metadata.reponame.replace("/", "-")}")(
                    s"${res.metadata.reponame}")))
              )
            ),
            div(`class` := "col s5")(
              results.map(
                res =>
                  div(id := s"${res.metadata.reponame.replace("/", "-")}")(div(
                    table(
                      thead(b(s"Call sites for ${res.metadata.reponame}")),
                      tbody(tr(td(div(
                        p(pprint(res.result)(HExtractionResult, res.metadata))
                      ))))
                    )
                  )))
            ),
            div(`class` := "col s4 offset-s6")(
              iframe(src := "https://github.com/")()
            )
          ))

        /*
          h1 (s"Call Sites for ${metadata.name}"),
        h4(a(href := metadata.url)(metadata.url)),
        div(`class` := "container")(p(pprint(x)(HExtractionResult, metadata)))
       */
      )
      // Footer
    ).render
  }

  def createOnClickMaybe(location: Option[Location],
                         metadata: ProjectMetadata) = location match {
    case Some(l) =>
      Seq(
        onclick := s"changeCode('${HLocation.composeGistItURL(l, metadata)}', ${l.line})",
        style := "text-color:blue;text-decoration:underline;")
    case None => Seq(onclick := "")
  }

  def createDocument[A](results: Seq[A], generator: HTMLDocument[A]): String = {
    html(
      head(
        link(rel := "stylesheet",
             href := "https://www.w3schools.com/w3css/4/w3.css")
      ),
      body(
        div(`class` := "w3-sidebar w3-bar-block", style := "width:20%")(
          generator.sidebar(results)
        ),
        div(`class` := "content", style := "margin-left:20%")(
          div(style := "width:80%")(
            generator.body(results)
          )
        )
      )
    ).render
  }

  trait HTMLDocument[A] {
    def sidebar(data: Seq[A]): Seq[HTMLTag]
    def body(data: Seq[A]): Seq[HTMLTag]
  }

  object CoderefDocument$ extends HTMLDocument[ProjectReport] {
    override def sidebar(data: Seq[ProjectReport]): Seq[HTMLTag] =
      data.map(
        res =>
          a(href := s"#${res.metadata.reponame.replace("/", "-")}",
            `class` := "w3-bar-item w3-button")(s"${res.metadata.reponame}"))

    override def body(results: Seq[ProjectReport]): Seq[HTMLTag] =
      Seq(
        script(raw(s"""
                        |function changeCode(what, line) {
                        |  document.write = function(what) { document.getElementById('frame').innerHTML += what }
                        |  var x = document.createElement("SCRIPT");
                        |  x.src = what
                        |  document.getElementById('frame').innerHTML = ""
                        |  document.getElementById('frame').appendChild(x)
                        |}
          """.stripMargin)),
        div(style := "width:50%")(
          results.map(
            res =>
              div(id := s"${res.metadata.reponame.replace("/", "-")}")(div(
                table(style := "word-wrap:break-word")(
                  b(s"Call sites for ${res.metadata.reponame}"),
                  ul(res.result.callSites.map(cs =>
                    tr(td(
                      cs.location match {
                        case Some(l) =>
                          div(
                            li(
                              createOnClickMaybe(cs.location, res.metadata): _*)(
                              s"${cs.code}${PrettyLocation
                                .pretty(cs.location.map(l => l.copy(file = "")), 0)}"),
                            li(createOnClickMaybe(cs.declaration.location,
                                                  res.metadata): _*)(
                              s"->${cs.declaration.kind} ${cs.declaration.name}${PrettyLocation
                                .pretty(cs.declaration.location.map(l => l.copy(file = "")), 0)}")
                          )
                        case None => li(cs.name)
                      }
                    ))))
                )
              )))
        ),
        div(
          style := "width: 35%; background-color: white; z-index: 10; position: fixed; right: 20px; top: 0px; height: 100%;")(
          p(b("Code Surrounding it:")),
          div(style := "width:100%;", id := "frame")(
            script(
              src := "http://gist-it.appspot.com/https://github.com/sksamuel/elastic4s/blob/c3bb17504a2d0d902e02c2a57b1873181f824e22/elastic4s-testkit/src/main/scala/com/sksamuel/elastic4s/testkit/HttpElasticSugar.scala?slice=10:20"
            )
          ),
          div(style := "position:fixed; top: 8.4em; right: 36%")(
            h2(">")
          )
        )
      )
  }

  object SummaryDocument$ extends HTMLDocument[ProjectReport] {
    override def sidebar(data: Seq[ProjectReport]): Seq[HTMLTag] =
      Seq(
        a(href := s"#summary", `class` := "w3-bar-item w3-button")(
          "all/summary")
      ) ++
        data.map(
          res =>
            a(href := s"#${res.metadata.reponame.replace("/", "-")}",
              `class` := "w3-bar-item w3-button")(s"${res.metadata.reponame}"))

    override def body(results: Seq[ProjectReport]): Seq[HTMLTag] = {
      def cellMaybe(what: Option[(String, Int)]) = what match {
        case Some(thing) => Seq(td(thing._2), td(thing._1))
        case None => Seq(td(""), td(""))
      }

      def printSummary(summary: ReportSummary) = {
        div(id := s"${summary.reponame.replace("/", "-")}")(
          b(s"Call sites for ${summary.reponame}"),
          table(`class` := "w3-table w3-bordered")(
            thead(
              td(s"${summary.totalCallSites}"),
              td(b("Call Sites"))
            ),
            tbody(
              summary.sortedCallSites
                .map(row => {
                  tr(td(row.occurrences), td(row.name))
                })
                .toSeq
            )
          )
        )
      }
      val projectSummaries = results.map(res => ReportSummary(res))
      val overallSummary = ReportSummary(projectSummaries)
      Seq(
        div(id := "summary", `class` := "w3-table w3-bordered")()(
          h4(b("Summary")),
          printSummary(
            overallSummary.copy(callSites = overallSummary.sortedCallSites))
        )) ++
        projectSummaries.map(printSummary)
    }
  }

  case class TCItem(defn: Declaration, metadata: ProjectMetadata)
  case class TCFamily(parent: TCItem, instances: Seq[TCItem])
  object TCListDocument extends HTMLDocument[TCFamily] {
    def kindedName(what: Declaration) = s"${what.kind} ${what.name}"

    override def sidebar(data: Seq[TCFamily]): Seq[HTMLTag] = {
      Seq(
        a(href := s"#summary", `class` := "w3-bar-item w3-button")(
          "all/summary")
      ) ++
        data
          .groupBy(_.parent.metadata)
          .keys
          .map(res =>
            a(href := s"#${res.reponame.replace("/", "-")}",
              `class` := "w3-bar-item w3-button")(s"${res.reponame}"))
    }

    override def body(data: Seq[TCFamily]): Seq[HTMLTag] = {
      def printInstances(items: Seq[TCItem]): HTMLTag = {
        ul(
          items.map(
            item =>
              li(
                a(href := HLocation.composeGHURL(item.defn.location.get,
                                                 item.metadata))(
                  kindedName(item.defn)
                )))
        )
      }

      def printFamily(family: TCFamily) = {
        div(
          b(
            a(href := HLocation.composeGHURL(family.parent.defn.location.get,
                                             family.parent.metadata))(
              kindedName(family.parent.defn))),
          div(
            b("Internal Instances"),
            printInstances(
              family.instances.filter(_.metadata == family.parent.metadata))
          ),
          div(
            b("External Instances"),
            printInstances(
              family.instances.filter(_.metadata != family.parent.metadata))
          )
        )
      }
      Seq(
        h3("Type classes per project"),
        div(
          data
            .groupBy(_.parent.metadata)
            .map(project => {
              div(id := s"${project._1.reponame.replace("/", "-")}")(
                h4(s"Type Classes defined in ${project._1.reponame}"),
                table(`class` := "w3-table w3-bordered")(
                  tbody(
                    project._2.map(fam => tr(td(printFamily(fam))))
                  ))
              )
            })
            .toSeq)
      )
    }
  }
}
