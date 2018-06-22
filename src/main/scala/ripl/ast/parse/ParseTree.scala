package ripl.ast.parse

trait Exp // also extended by ast/common/atom

case object SExp { def apply(exps: Exp*): SExp = SExp(exps.toList) }
case class SExp(exps: List[Exp]) extends Exp
