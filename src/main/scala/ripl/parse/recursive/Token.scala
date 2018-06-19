package ripl.parse.recursive

sealed trait Token
case object Token {
  case object Newline          extends Token
  case object LParen           extends Token
  case object RParen           extends Token

  sealed trait PrefixOperator extends Token
  case object Apostrophe extends PrefixOperator
  case object Tilda extends PrefixOperator
  case object Circumflex extends PrefixOperator

  case class Symbol(s: String) extends Token

  // value atoms
  case class Bool(v: Boolean)  extends Token
  case class Float(v: Float)   extends Token
  case class Int(v: Long)      extends Token
  case class String(v: String) extends Token
}


