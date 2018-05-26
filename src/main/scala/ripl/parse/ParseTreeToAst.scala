package ripl.parse

import org.antlr.v4.runtime.ParserRuleContext

import ripl.parser.antlr.{riplParser => rp}
import ripl.ast.common._
import ripl.ast.untyped._

case object ParseTreeToAst {

  def apply(context: ParserRuleContext): Node = context match {
    case c: rp.Exp0Context => mapExp0(c)
    case c: rp.Exp1Context => mapExp1(c)
    case c: rp.Exp2Context => mapExp2(c)
  }

  def mapExp0(c: rp.Exp0Context): Exp = c match {
    case n: rp.NameContext => mapName(n)
    case n: rp.BlnContext => mapBln(n)
    case n: rp.IntContext => mapInt(n)
    case n: rp.FltContext => mapFlt(n)
    case n: rp.BracketExpContext => mapBracketExp(n)

    case n: rp.PlusContext => Name("+")
    case n: rp.MinusContext => Name("-")
    case n: rp.StarContext => Name("*")
    case n: rp.SlashContext => Name("/")
    case n: rp.PercentContext => Name("%")
  }

  def mapName(c: rp.NameContext): Name
    = Name(c.Name().getText)

  def mapBln(c: rp.BlnContext): VBln
    = VBln(c.VBln().getText.toBoolean)

  def mapInt(c: rp.IntContext): VInt
    = VInt(c.VInt().getText.toInt)

  def mapFlt(c: rp.FltContext): VFlt
    = VFlt(c.getText.toFloat)

  def mapBracketExp(c: rp.BracketExpContext): Exp
    = mapExp2(c.exp2())


  def mapExp1(c: rp.Exp1Context): Exp = c match {
    case n: rp.NegateContext => mapNegate(n)
    case n: rp.AddContext => mapAddition(n)
    case n: rp.MultiplyContext => mapMultiplication(n)
    case n: rp.BinOpContext => mapBinOp(n)
    case n: rp.FunTypeContext => mapFunType(n)
    case n: rp.FunContext => mapFun(n)
    case n: rp.ApplyContext => mapApply(n)
    case n: rp.Exp10Context => mapExp0(n.exp0)
  }

  def mapNegate(c: rp.NegateContext): App =
    App(Name("-"),
      mapExp1(c.exp1()))

  def mapAddition(c: rp.AddContext): App =
    App(Name("+"),
      mapExp1(c.exp1(0)),
      mapExp1(c.exp1(1)))

  def mapMultiplication(c: rp.MultiplyContext): App =
    App(Name("*"),
      mapExp1(c.exp1(0)),
      mapExp1(c.exp1(1)))

  def mapBinOp(c: rp.BinOpContext): App =
    App(
      mapExp0(c.exp0()),
      mapExp1(c.exp1(0)),
      mapExp1(c.exp1(1)))

  def mapFunType(c: rp.FunTypeContext): TFun =
    TFun(
      mapFunParamTypes(c.funTypeParams()),
      mapExp1(c.exp1()))

  def mapFun(c: rp.FunContext): Fun = c.exp1().size match {
    case 1 =>
      Fun(mapParams(c.params()), None, mapExp1(c.exp1(0)))
    case 2 =>
      Fun(mapParams(c.params()), Some(mapExp1(c.exp1(0))), mapExp1(c.exp1(1)))
  }

  def mapIf(c: rp.IfContext): If =
    If(
      mapExp1(c.exp1(0)),
      mapExp1(c.exp1(1)),
      mapExp2(c.exp2))

  def mapApply(c: rp.ApplyContext): App =
    App(
      mapExp0(c.exp0()),
      mapExps(c.exps()))

  def mapExp2(c: rp.Exp2Context): Exp = c match {
    case n: rp.Exp21Context => mapExp1(n.exp1)
    case n: rp.IfContext => mapIf(n)
  }

  def mapExps(c: rp.ExpsContext): List[Exp] =
    scala.collection.JavaConverters.asScalaBuffer(c.exp2()).map(mapExp2).toList

  def mapFunParamTypes(c: rp.FunTypeParamsContext): List[Exp] =
    c match {
      case c: rp.FunTypeParamExpContext => List(mapExp0(c.exp0()))
      case c: rp.FunTypeParamExpsContext => mapExps(c.exps())
    }

  def mapParams(c: rp.ParamsContext): List[Param] = c match {
    case null => Nil
    case params => scala.collection.JavaConverters.asScalaBuffer(c.param())
      .map(mapParam)
      .toList
  }

  def mapParam(c: rp.ParamContext): Param = c match {
    case c: rp.ParamDoubleContext =>
      Param(
        mapExp0(c.exp0(1)) match { case Name(n) => n; case _ => "ExpectedName" },
        mapExp0(c.exp0(0)))
  }
}

