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
        case SExp(Name(n) :: params) :: value :: Nil =>
          (n, Fun(params.map(mapParam), None, mapExp(value)))
        case SExp(Name(n) :: params) :: returnType :: value :: Nil =>
          (
            n,
            Fun(params.map(mapParam), Some(mapExp(returnType)), mapExp(value))
          )
      }
    case SExp(Name("external") :: rest) =>
      rest match {
        case SExp(Name(n) :: params) :: returnType :: Nil =>
          (
            n,
            External(params.map(mapParam), mapExp(returnType))
          )
      }
    case SExp(Name("struct") :: rest) =>
      rest match {
        case Name(name) :: fields =>
          (name, Struct(name, fields.map(mapStructMember)))
      }
  }

  private def mapExp(exp: p.Exp): Exp = exp match {
    case a: Atom =>
      a match {
        // TODO: I don't really like this here,
        // but I don't really know where else it should go either
        case Name("true")  => VBln(true)
        case Name("false") => VBln(false)
        case Name("none")  => VNone
        case _             => a
      }
    case SExp(exps) =>
      exps match {
        case Name("block") :: exps =>
          Block(exps.map(mapExp))
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
        case (f :: exps) => App(mapExp(f), exps.map(mapExp))
      }
    case p.Select(a, b) =>
      Select(mapExp(a), mapExp(b) match { case Name(n) => n; case _ => ??? })
  }

  private def mapParam(exp: p.Exp): Param = exp match {
    case SExp(t :: Name(n) :: Nil) => Param(n, mapExp(t))
    case _                         => ???
    // TODO: error for invalid parameter form
  }

  // TODO: use same type for struct members and parameters
  private def mapStructMember(exp: p.Exp): (String, Exp) = exp match {
    case SExp(t :: Name(n) :: Nil) => (n, mapExp(t))
    case _                         => ???
    // TODO: error for invalid parameter form
  }

}
