package ripl.parser

import ripl.parser.antlr._
import ripl.ast.common._
import ripl.ast.untyped._

class ParseTreeVisitor extends riplBaseVisitor[Node] {

  override def visitName(c: riplParser.NameContext): Name
    = Name(c.Name().getText)

  override def visitBln(c: riplParser.BlnContext): VBln
    = VBln(c.VBln().getText.toBoolean)

  override def visitInt(c: riplParser.IntContext): VInt
    = VInt(c.VInt().getText.toInt)

  override def visitFlt(c: riplParser.FltContext): VFlt
    = VFlt(c.getText.toFloat)
}

