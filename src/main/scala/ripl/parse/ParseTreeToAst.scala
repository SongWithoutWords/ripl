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
    case c: rp.NameContext => Name(c.Name().getText)
    case c: rp.BlnContext => VBln(c.VBln().getText.toBoolean)
    case c: rp.IntContext => VInt(c.VInt().getText.toInt)
    case c: rp.FltContext => VFlt(c.getText.toFloat)
    case c: rp.BracketExpContext => mapExp2(c.exp2())

    // Catch-all for tokens that can be referenced by name
    case _ => Name(c.getText())
  }

  def mapExp1(c: rp.Exp1Context): Exp = c match {

    case c: rp.NegateContext =>
      App(
        Name("-"),
        mapExp1(c.exp1()))

    case c: rp.AddContext =>
      App(
        Name("+"),
        mapExp1(c.exp1(0)),
        mapExp1(c.exp1(1)))

    case c: rp.MultiplyContext =>
      App(
        Name("*"),
        mapExp1(c.exp1(0)),
        mapExp1(c.exp1(1)))

    case c: rp.BinOpContext =>
      App(
        mapExp0(c.exp0()),
        mapExp1(c.exp1(0)),
        mapExp1(c.exp1(1)))

    case c: rp.FunTypeContext =>
      TFun(
        mapFunParamTypes(c.funTypeParams()),
        mapExp1(c.exp1()))

    case c: rp.FunContext => c.exp1().size match {
      case 1 =>
        Fun(
          mapParams(c.params()),
          None,
          mapExp1(c.exp1(0)))
      case 2 =>
        Fun(
          mapParams(c.params()),
          Some(mapExp1(c.exp1(0))),
          mapExp1(c.exp1(1)))
    }

    case c: rp.ApplyContext =>
      App(
        mapExp0(c.exp0()),
        mapExps(c.exps()))

    case n: rp.Exp10Context =>
      mapExp0(n.exp0)
  }

  def mapExp2(c: rp.Exp2Context): Exp = c match {
    case c: rp.IfContext =>
      If(
        mapExp1(c.exp1(0)),
        mapExp1(c.exp1(1)),
        mapExp2(c.exp2))
    case n: rp.Exp21Context => mapExp1(n.exp1)
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

