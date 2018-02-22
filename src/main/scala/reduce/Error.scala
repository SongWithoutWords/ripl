package reduce

import ast.Type

sealed trait Error

case class ApplicationOfNonAppliableType(t: Type) extends Error
case class RecursiveVariableDef() extends Error
case class UnknownName(n: String) extends Error
case class WrongNumArgs() extends Error

