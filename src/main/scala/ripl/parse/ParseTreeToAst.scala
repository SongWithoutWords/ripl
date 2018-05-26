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
    case n: riplParser.AddContext => mapAddition(n)
    case n: riplParser.MultiplyContext => mapMultiplication(n)
    case n: riplParser.BinOpContext => mapBinOp(n)
    case n: riplParser.FunTypeContext => mapFunType(n)
    case n: riplParser.FunContext => mapFun(n)
    case n: riplParser.ApplyContext => mapApply(n)
    case n: riplParser.Exp10Context => mapExp0(n.exp0)
  }

  def mapNegate(c: riplParser.NegateContext): App =
    App(Name("-"),
      mapExp1(c.exp1()))

  def mapAddition(c: riplParser.AddContext): App =
    App(Name("+"),
      mapExp1(c.exp1(0)),
      mapExp1(c.exp1(1)))

  def mapMultiplication(c: riplParser.MultiplyContext): App =
    App(Name("*"),
      mapExp1(c.exp1(0)),
      mapExp1(c.exp1(1)))

  def mapBinOp(c: riplParser.BinOpContext): App =
    App(
      mapExp0(c.exp0()),
      mapExp1(c.exp1(0)),
      mapExp1(c.exp1(1)))

  def mapFunType(c: riplParser.FunTypeContext): TFun =
    TFun(
      mapFunParamTypes(c.funTypeParams()),
      mapExp1(c.exp1()))

  def mapFun(c: riplParser.FunContext): Fun = c.exp1().size match {
    case 1 =>
      Fun(mapParams(c.params()), None, mapExp1(c.exp1(0)))
    case 2 =>
      Fun(mapParams(c.params()), Some(mapExp1(c.exp1(0))), mapExp1(c.exp1(1)))
  }

  def mapIf(c: riplParser.IfContext): If =
    If(
      mapExp1(c.exp1(0)),
      mapExp1(c.exp1(1)),
      mapExp2(c.exp2))

  def mapApply(c: riplParser.ApplyContext): App =
    App(
      mapExp0(c.exp0()),
      mapExps(c.exps()))

  def mapExp2(c: riplParser.Exp2Context): Exp = c match {
    case n: riplParser.Exp21Context => mapExp1(n.exp1)
    case n: riplParser.IfContext => mapIf(n)
  }

  def mapExps(c: riplParser.ExpsContext): List[Exp] =
    scala.collection.JavaConverters.asScalaBuffer(c.exp2()).map(mapExp2).toList

  def mapFunParamTypes(c: riplParser.FunTypeParamsContext): List[Exp] =
    c match {
      case c: riplParser.FunTypeParamExpContext => List(mapExp0(c.exp0()))
      case c: riplParser.FunTypeParamExpsContext => mapExps(c.exps())
    }

  def mapParams(c: riplParser.ParamsContext): List[Param] = c match {
    case null => Nil
    case params => scala.collection.JavaConverters.asScalaBuffer(c.param())
      .map(mapParam)
      .toList
  }

  def mapParam(c: riplParser.ParamContext): Param = c match {
    case c: riplParser.ParamDoubleContext =>
      Param(
        mapExp0(c.exp0(1)) match { case Name(n) => n; case _ => "ExpectedName" },
        mapExp0(c.exp0(0)))
  }
}

