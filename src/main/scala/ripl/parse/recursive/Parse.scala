package ripl.parse.recursive

import ripl.ast.untyped._

case object Parse {
  def apply(input: String): Ast      = Parse(Lex(input))
  def apply(input: List[Token]): Ast = ???
}
