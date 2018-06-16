// Predicates for the 'LLVM.AST.Instruction.ICmp' instruction
package ripl.llvm.pure.ast

// <http://llvm.org/docs/LangRef.html#icmp-instruction>
sealed trait IntegerPredicate

case object IntegerPredicate {
  case object EQ extends IntegerPredicate
  case object NE extends IntegerPredicate
  case object UGT extends IntegerPredicate
  case object UGE extends IntegerPredicate
  case object ULT extends IntegerPredicate
  case object ULE extends IntegerPredicate
  case object SGT extends IntegerPredicate
  case object SGE extends IntegerPredicate
  case object SLT extends IntegerPredicate
  case object SLE extends IntegerPredicate
}
