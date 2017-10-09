package implicits

import scalafix._
import scala.meta._

final case class ShowTheImplicits()
  extends Rule("ShowTheImplicits") {

  override def fix(ctx: RuleCtx): Patch = {
    ctx.tree.collect {
      case x : Mod.Implicit => {
        val filename = ctx.input match {
          case virtualFile: Input.VirtualFile => virtualFile.path
          case regularFile: Input.File => regularFile.path
          case _ => s"Input type Unknown.\n Here is the full Input: \n ${ctx.input}"
        }
        println(s"Implicit found (with FIX) at [line: ${x.pos.startLine}, col: ${x.pos.startColumn}], in file ${filename}")
      }
    }
    Patch.empty
  }

  /* Do not activate the check until sclafix-cli has been fixed
  override def check(ctx: RuleCtx): List[LintMessage] = {    
    val implicitAppearances = LintCategory.warning("Implicit Appeared (with CHECK)")
    ctx.tree.collect {
      case x : Mod.Implicit => {
        implicitAppearances.at(x.pos)
      }
    }
  }
  */
}
