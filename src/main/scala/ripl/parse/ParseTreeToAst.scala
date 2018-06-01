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
      case _ => "ExpectedName" }

    def pairToParam(pair: (String, Exp)) = Param(pair._1, pair._2)
  }
  import Util._

  def mapExp0(c: rp.Exp0Context): Exp = c match {
    case c: rp.NameContext => Name(c.Name().getText)
    case c: rp.BlnContext => VBln(c.VBln().getText.toBoolean)
    case c: rp.IntContext => VInt(c.VInt().getText.toInt)
    case c: rp.FltContext => VFlt(c.getText.toFloat)
    case c: rp.StrContext => VStr(c.getText.stripPrefix("\"").stripSuffix("\""))
    case c: rp.BracketExpContext => mapExp2(c.e)

    case c: rp.ApplyContext =>
      App(
        mapExp0(c.f),
        mapExps(c.args))

    case c: rp.SelectContext =>
      Select(
        mapExp0(c.e1),
        expToNameString(mapExp0(c.e2)))

    case c: rp.UnaryOpContext =>
      App(
        Name(c.op.getText()),
        mapExp0(c.e))

    // Use operator token text as the names of the operators
    case _ => Name(c.getText())
  }

  def mapExp1(c: rp.Exp1Context): Exp = c match {


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

    case c: rp.BlockContext =>
      Block(asScalaBuffer(c.es).map(mapExp2).toList)

    case c: rp.E1DefineContext =>
      mapDefine(c.define())

    case c: rp.E1LambdaContext =>
      mapLambda(c.lambda())

    case c: rp.E1UserTypeContext =>
      mapUserType(c.userType())

    case c: rp.AssignContext =>
      Assign(
        mapExp1(c.lhs),
        mapExp2(c.rhs))

    case n: rp.Exp10Context =>
      mapExp0(n.exp0)
  }

  def mapDefine(c: rp.DefineContext) = {
    val pair = mapPair(c.lhs)
    Define(
      pair._2,
      Cons(pair._1, mapExp2(c.rhs)))
  }

  def mapLambda(c: rp.LambdaContext) =
    Fun(
      asScalaBuffer(c.params)
        .map(pair => pairToParam(mapPairToNamedExp(pair)))
        .toList,
      c.returnType match {
        case null => None
        case rt => Some(mapExp1(rt))
      },
      mapExp2(c.exp))

  def mapFunction(c: rp.FunctionContext) =
    Define(mapExp0(c.name), mapLambda(c.lambda()))

  def mapUserType(c: rp.UserTypeContext) = c match {
    case c: rp.DataContext =>
      Struct(
        expToNameString(mapExp0(c.name)),
        MultiMap(asScalaBuffer(c.fields).map(mapPairToNamedExp): _*))

    case c: rp.UnionContext =>
      Union(
        expToNameString(mapExp0(c.name)),
        asScalaBuffer(c.alternatives).map(mapExp2).toList)
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

  def mapPair(c: rp.PairContext) =
    ( mapExp0(c.e1)
    , mapExp0(c.e2))

  def mapPairToNamedExp(c: rp.PairContext): (String, Exp) =
    ( expToNameString(mapExp0(c.e2))
    , mapExp0(c.e1))

  def mapUnit(c: rp.UnitContext): (String, Node) = c match {

    case c: rp.UnitDefineContext =>
      val (name, typ) = mapPairToNamedExp(c.define().lhs)
      (name, Cons(typ, mapExp2(c.define().rhs)))

    case c: rp.UnitFunctionContext =>
      val Define(name, lambda) = mapFunction(c.function())
      (expToNameString(name), lambda)

    case c: rp.UnitUserTypeContext =>
      val userType = mapUserType(c.userType())
      (userType.name, userType)
  }

  def mapAst(c: rp.AstContext): List[(String, Node)] =
    asScalaBuffer(c.units).map(mapUnit).toList
}

