package ripl.codegen

import ripl.ast.typed._
import ripl.llvm.{ast => l}

case object CodeGen {
  def apply(ast: Ast): l.Module = genModule(ast)

  def genModule(ast: Ast): l.Module = {
    val definitions = ast.toList.map { case (name, node) => genUnit(name, node) }
    l.Module("ripl", "", None, None, definitions)
  }

  def genUnit(name: String, node: Node): l.Definition = l.GlobalDefinition(
    node match {
      case Fun(params, rType, exp) =>
        l.Function(
          callingConvention = l.CallingConvention.Fast,
          returnType = genType(rType),
          name = l.Name(name),
          parameters = l.Parameters(params.map(genParam))
        )
    }
  )

  def genType(t: Type): l.Type = t match {
    case _ => ???
  }

  def genParam(p: Param): l.Parameter = l.Parameter(genType(p.t), l.Name(p.n))
}
