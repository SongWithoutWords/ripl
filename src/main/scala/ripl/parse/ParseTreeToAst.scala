package ripl.parse

import scala.collection.JavaConverters.asScalaBuffer

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
    case c: rp.BracketExpContext => mapExp2(c.e)

    // Use operator token text as the names of the operators
    case _ => Name(c.getText())
  }

  def mapExp1(c: rp.Exp1Context): Exp = c match {

    case c: rp.UnaryOpContext =>
      App(
        Name(c.op.getText()),
        mapExp1(c.e))

    case c: rp.BinOpMulDivModContext =>
      App(
        Name(c.op.getText()),
        mapExp1(c.e1),
        mapExp1(c.e2))

    case c: rp.BinOpAddSubContext =>
      App(
        Name(c.op.getText()),
        mapExp1(c.e1),
        mapExp1(c.e2))

    case c: rp.BinOpCompareContext =>
      App(
        Name(c.op.getText()),
        mapExp1(c.e1),
        mapExp1(c.e2))

    case c: rp.BinOpAndContext =>
      App(
        Name("and"),
        mapExp1(c.e1),
        mapExp1(c.e2))

    case c: rp.BinOpOrContext =>
      App(
        Name("or"),
        mapExp1(c.e1),
        mapExp1(c.e2))

    case c: rp.BinOpContext =>
      App(
        mapExp0(c.op),
        mapExp1(c.e1),
        mapExp1(c.e2))

    case c: rp.FunTypeContext =>
      TFun(
        mapFunParamTypes(c.funTypeParams()),
        mapExp1(c.exp1()))

    case c: rp.FunContext =>
      Fun(
        asScalaBuffer(c.params)
          .map(mapParam)
          .toList,
        c.returnType match {
          case null => None
          case rt => Some(mapExp1(rt))
        },
        mapExp1(c.exp))

    case c: rp.ApplyContext =>
      App(
        mapExp0(c.f),
        mapExps(c.args))

    case c: rp.SelectContext =>
      Select(
        mapExp0(c.e1),
        mapExp0(c.e2) match { case Name(n) => n; case _ => "ExpectedName" })

    case n: rp.Exp10Context =>
      mapExp0(n.exp0)
  }

  def mapExp2(c: rp.Exp2Context): Exp = c match {
    case c: rp.IfContext =>
      If(
        mapExp1(c.e1),
        mapExp1(c.e2),
        mapExp2(c.e3))
    case n: rp.Exp21Context => mapExp1(n.exp1)
  }

  def mapExps(c: rp.ExpsContext): List[Exp] =
    asScalaBuffer(c.exp2()).map(mapExp2).toList

  def mapFunParamTypes(c: rp.FunTypeParamsContext): List[Exp] =
    c match {
      case c: rp.FunTypeParamExpContext => List(mapExp0(c.exp0()))
      case c: rp.FunTypeParamExpsContext => mapExps(c.exps())
    }

  def mapParam(c: rp.ParamContext): Param = c match {
    case c: rp.ParamDoubleContext =>
      Param(
        mapExp0(c.exp0(1)) match { case Name(n) => n; case _ => "ExpectedName" },
        mapExp0(c.exp0(0)))
  }
}

