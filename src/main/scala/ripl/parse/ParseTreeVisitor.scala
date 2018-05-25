package ripl.parser

import ripl.parser.antlr._
import ripl.ast.common._
import ripl.ast.untyped._

class ParseTreeVisitor extends riplBaseVisitor[Node] {

  def visitExp0(c: riplParser.Exp0Context): Exp = c match {
    case n: riplParser.NameContext => visitName(n)
    case n: riplParser.BlnContext => visitBln(n)
    case n: riplParser.IntContext => visitInt(n)
    case n: riplParser.FltContext => visitFlt(n)
  }

  override def visitName(c: riplParser.NameContext): Name
    = Name(c.Name().getText)

  override def visitBln(c: riplParser.BlnContext): VBln
    = VBln(c.VBln().getText.toBoolean)

  override def visitInt(c: riplParser.IntContext): VInt
    = VInt(c.VInt().getText.toInt)

  override def visitFlt(c: riplParser.FltContext): VFlt
    = VFlt(c.getText.toFloat)


  def visitExp1(c: riplParser.Exp1Context): Exp = c match {
    case n: riplParser.AdditionContext => visitAddition(n)
    case n: riplParser.MultiplicationContext => visitMultiplication(n)
    case n: riplParser.Exp10Context => visitExp0(n.exp0)
  }

  override def visitAddition(c: riplParser.AdditionContext): App =
    App(Name("+"),
      visitExp1(c.exp1(0)),
      visitExp1(c.exp1(1)))

  override def visitMultiplication(c: riplParser.MultiplicationContext): App =
    App(Name("*"),
      visitExp1(c.exp1(0)),
      visitExp1(c.exp1(1)))
}

