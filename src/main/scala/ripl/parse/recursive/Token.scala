package ripl.parse.recursive

trait Token // also extended by ValAtom
case object Token {
  case object Newline extends Token
  case object LParen  extends Token
  case object RParen  extends Token

  sealed trait PrefixOperator extends Token
  case object Apostrophe      extends PrefixOperator
  case object Circumflex      extends PrefixOperator
  case object Tilda           extends PrefixOperator

  case class Symbol(s: String) extends Token
}
