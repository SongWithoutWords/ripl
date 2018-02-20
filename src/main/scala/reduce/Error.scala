package reduce

sealed trait Error

case class RecursiveVariableDef() extends Error
case class UnknownName(n: String) extends Error
case class WrongNumArgs() extends Error

