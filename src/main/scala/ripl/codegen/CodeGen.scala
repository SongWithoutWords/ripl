package ripl.codegen

import ripl.ast.{typed => t}
import ripl.llvm.{ast => l}

case object CodeGen {
  def apply(a: t.Ast): l.Module = ???
}
