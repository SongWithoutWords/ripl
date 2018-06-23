package ripl.reduce

import ripl.ast.{untyped => a0}
import ripl.ast.{typed => a1}

sealed trait Error

case class AmbiguousImplicitConversions(conversions: List[a1.Exp]) extends Error
case class AmbiguousOverload[A](overloads: List[ReduceM[A]])       extends Error
case class AmbiguousType(input: a0.Exp)                            extends Error
case class AmbiguousUnit(overloads: List[ReduceM[a1.Node]])        extends Error
case class ApplicationOfNonAppliableType(t: a1.Type)               extends Error
case class NonExistentMember(n: String)                            extends Error
case class RecursiveFunctionLacksExplicitReturnType(cycle: a1.Cycle)
    extends Error
case class RecursiveVariableDef(cycle: a1.Cycle)           extends Error
case class RequiredExp(found: a1.Node)                     extends Error
case class RequiredType(input: a0.Exp)                     extends Error
case class SelectionFromNonStructType(t: a1.Type)          extends Error
case class SelectionFromNonSelectable(n: a1.Node)          extends Error
case class TypeConflict(expected: a1.Type, found: a1.Type) extends Error // Should be no-implicitConversion?
case class UnknownName(n: String)                          extends Error
case object UseOfInvalidExp                                extends Error
case class WrongNumArgs(expected: Int, found: Int)         extends Error
