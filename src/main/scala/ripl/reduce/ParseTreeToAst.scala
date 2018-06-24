package ripl.reduce

import ripl.ast.common._
import ripl.ast.{parse => p}
import ripl.ast.parse.SExp
import ripl.ast.untyped._
import ripl.util.MultiMap

case object ParseTreeToAst {

  def apply(exps: List[p.Exp]): Definitions = mapDefinitions(exps)

  private def mapDefinitions(exps: List[p.Exp]): Definitions = {
    MultiMap(exps.map(mapDefinition): _*)
  }

  private def mapDefinition(exp: p.Exp): (String, Exp) = exp match {
    case SExp(Name("namespace") :: rest) =>
      rest match {
        case Name(n) :: members => (n, Namespace(mapDefinitions(members)))
        case _                  => ??? // TODO: namespace without a name is an error!
      }
    case SExp(Name("define") :: rest) =>
      rest match {
        case Name(n) :: value :: Nil => (n, mapExp(value))
      }
  }

  private def mapExp(exp: p.Exp): Exp = exp match {
    case a: Atom => a
    case SExp(exps) =>
      exps match {
        case Name("if") :: a :: b :: c :: Nil =>
          If(mapExp(a), mapExp(b), mapExp(c))
        case Name("set") :: a :: b :: Nil => Assign(mapExp(a), mapExp(b))
        case Name("lambda") :: rest =>
          rest match {

            case SExp(params) :: returnType :: value :: Nil =>
              Fun(params.map(mapParam), Some(mapExp(returnType)), mapExp(value))

            case SExp(params) :: value :: Nil =>
              Fun(params.map(mapParam), None, mapExp(value))

            case _ => ???
            // TODO: error for invalid function form

          }
        case _ => ???
      }
  }

  private def mapParam(exp: p.Exp): Param = exp match {
    case SExp(Name(n) :: t :: Nil) => Param(n, mapExp(t))
    case _                         => ???
    // TODO: error for invalid parameter form
  }
}
