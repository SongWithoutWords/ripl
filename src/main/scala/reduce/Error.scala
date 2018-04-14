package reduce

import reduce.ast.{untyped => a0}
import reduce.ast.{typed => a1}

sealed trait Error

case class AmbiguousType(input: a0.Node) extends Error
case class AmbiguousOverload[A](overloads: List[ReduceM[A]]) extends Error
case class AmbiguousUnit(overloads: List[ReduceM[a1.Node]]) extends Error
case class ApplicationOfNonAppliableType(t: a1.Type) extends Error
case class NonExistentMember(n: String) extends Error
case class RecursiveVariableDef(n: a0.Node) extends Error
case class RequiredExp(found: a1.Node) extends Error
case class RequiredType(input: a0.Node) extends Error
case class TypeConflict(expected: a1.Type, found: a1.Type) extends Error
case class UnknownName(n: String) extends Error
case class WrongNumArgs(expected: Int, found: Int) extends Error

