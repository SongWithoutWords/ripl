package ripl.parse

import scala.collection.JavaConverters.asScalaBuffer

import org.antlr.v4.runtime.ParserRuleContext

import ripl.parser.antlr.{RiplParser => rp}
import ripl.ast.common._
import ripl.ast.untyped._
import ripl.util.MultiMap

case object ParseTreeToAst {

  private object Util {
    def expToNameString(e: Exp): String = e match {
      case Name(n) => n;
      case _       => "ExpectedName"
    }

    def mapExps(es: java.util.List[rp.ExpContext]): Exp =
      SExp(asScalaBuffer(es).map(mapExp).toList)
  }
  import Util._

  def mapExp(c: rp.ExpContext): Exp = c match {
    case c: rp.NameContext => Name(c.Name().getText)
    case c: rp.BlnContext  => VBln(c.VBln().getText.toBoolean)
    case c: rp.IntContext  => VInt(c.VInt().getText.toInt)
    case c: rp.FltContext  => VFlt(c.getText.toFloat)
    case c: rp.StrContext  => VStr(c.getText.stripPrefix("\"").stripSuffix("\""))
    case c: rp.SExpContext => mapExps(c.es)

    case c: rp.SelectContext =>
      Select(mapExp(c.e1), expToNameString(mapExp(c.e2)))

    case c: rp.UnaryOpContext =>
      App(Name(c.op.getText()), mapExp(c.e))
  }
}
