package ripl.parse

import org.antlr.v4.runtime.ParserRuleContext

import ripl.parser.antlr._
import ripl.ast.common._
import ripl.ast.untyped._

case object ParseTreeToAst {

  def apply(context: ParserRuleContext): Node = context match {
    case c: riplParser.Exp0Context => mapExp0(c)
    case c: riplParser.Exp1Context => mapExp1(c)
    case c: riplParser.Exp2Context => mapExp2(c)
  }

  def mapExp0(c: riplParser.Exp0Context): Exp = c match {
    case n: riplParser.NameContext => mapName(n)
    case n: riplParser.BlnContext => mapBln(n)
    case n: riplParser.IntContext => mapInt(n)
    case n: riplParser.FltContext => mapFlt(n)
    case n: riplParser.BracketExpContext => mapBracketExp(n)

    case n: riplParser.PlusContext => Name("+")
    case n: riplParser.MinusContext => Name("-")
    case n: riplParser.StarContext => Name("*")
    case n: riplParser.SlashContext => Name("/")
    case n: riplParser.PercentContext => Name("%")
  }

  def mapName(c: riplParser.NameContext): Name
    = Name(c.Name().getText)

  def mapBln(c: riplParser.BlnContext): VBln
    = VBln(c.VBln().getText.toBoolean)

  def mapInt(c: riplParser.IntContext): VInt
    = VInt(c.VInt().getText.toInt)

  def mapFlt(c: riplParser.FltContext): VFlt
    = VFlt(c.getText.toFloat)

  def mapBracketExp(c: riplParser.BracketExpContext): Exp
    = mapExp2(c.exp2())


  def mapExp1(c: riplParser.Exp1Context): Exp = c match {
    case n: riplParser.NegateContext => mapNegate(n)
    case n: riplParser.AdditionContext => mapAddition(n)
    case n: riplParser.MultiplicationContext => mapMultiplication(n)
    case n: riplParser.Exp10Context => mapExp0(n.exp0)
  }

  def mapNegate(c: riplParser.NegateContext): App =
    App(Name("-"),
      mapExp1(c.exp1()))

  def mapAddition(c: riplParser.AdditionContext): App =
    App(Name("+"),
      mapExp1(c.exp1(0)),
      mapExp1(c.exp1(1)))

  def mapMultiplication(c: riplParser.MultiplicationContext): App =
    App(Name("*"),
      mapExp1(c.exp1(0)),
      mapExp1(c.exp1(1)))

  def mapExp2(c: riplParser.Exp2Context): Exp = c match {
    case n: riplParser.Exp21Context => mapExp1(n.exp1)
  }
}

