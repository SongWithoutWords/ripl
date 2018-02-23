package reduce

import ast._

sealed trait Error

case class ApplicationOfNonAppliableType(t: Type) extends Error
case class RecursiveVariableDef(n: Node) extends Error
case class TypeConflict(expected: Type, found: Type) extends Error
case class UnknownName(n: String) extends Error
case class WrongNumArgs(expected: Int, found: Int) extends Error

