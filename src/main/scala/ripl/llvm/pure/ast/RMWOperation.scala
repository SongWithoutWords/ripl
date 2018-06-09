// Operations for the 'LLVM.AST.Instruction.AtomicRMW' instruction
package ripl.llvm.pure.ast

// <http://llvm.org/docs/LangRef.html#atomicrmw-instruction>
sealed trait RMWOperation
case object RMWOperation {
  case object Xchg extends RMWOperation
  case object Add extends RMWOperation
  case object Sub extends RMWOperation
  case object And extends RMWOperation
  case object Nand extends RMWOperation
  case object Or extends RMWOperation
  case object Xor extends RMWOperation
  case object Max extends RMWOperation
  case object Min extends RMWOperation
  case object UMax extends RMWOperation
  case object UMin extends RMWOperation
}
