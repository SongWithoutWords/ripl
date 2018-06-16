// Predicates for the 'LLVM.AST.Instruction.FCmp' instruction
package ripl.llvm.ast

// <http://llvm.org/docs/LangRef.html#fcmp-instruction>

sealed trait FloatingPointPredicate

case object FloatingPointPredicate {
  case object False extends FloatingPointPredicate
  case object OEQ extends FloatingPointPredicate
  case object OGT extends FloatingPointPredicate
  case object OGE extends FloatingPointPredicate
  case object OLT extends FloatingPointPredicate
  case object OLE extends FloatingPointPredicate
  case object ONE extends FloatingPointPredicate
  case object ORD extends FloatingPointPredicate
  case object UNO extends FloatingPointPredicate
  case object UEQ extends FloatingPointPredicate
  case object UGT extends FloatingPointPredicate
  case object UGE extends FloatingPointPredicate
  case object ULT extends FloatingPointPredicate
  case object ULE extends FloatingPointPredicate
  case object UNE extends FloatingPointPredicate
  case object True extends FloatingPointPredicate
}
