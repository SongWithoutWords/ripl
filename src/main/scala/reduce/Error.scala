package reduce

import ast.Type

sealed trait Error

case class ApplicationOfNonAppliableType(t: Type) extends Error
object RecursiveVariableDef extends Error
case class UnknownName(n: String) extends Error
case class WrongNumArgs(expected: Int, found: Int) extends Error

