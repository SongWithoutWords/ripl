package ripl.llvm.pure.ast

// This module provides a sub-namespace for a type to support the various sizes of floating point
// numbers LLVM supports. It is most definitely intended to be imported qualified.

sealed trait SomeFloat
//   case class Half(value: Word16)
case class Single(value: Float) extends SomeFloat
case class Double(value: Float) extends SomeFloat
//   case class Quadruple(high: Word64, low: Word64)
//   case class X86_FP80(high: Word16, low: Word64)
//   case class PPC_FP128(high: Word64, low: Word64)
