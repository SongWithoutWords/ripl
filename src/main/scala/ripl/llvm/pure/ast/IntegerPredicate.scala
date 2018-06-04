// Predicates for the 'LLVM.AST.Instruction.ICmp' instruction
package ripl.llvm.pure.ast

// <http://llvm.org/docs/LangRef.html#icmp-instruction>
sealed trait IntegerPredicate

case object IntegerPredicate {
  case object EQ
  case object NE
  case object UGT
  case object UGE
  case object ULT
  case object ULE
  case object SGT
  case object SGE
  case object SLT
  case object SLE
}
