// A representation of an LLVM inline assembly
package ripl.llvm.pure.ast

// the dialect of assembly used in an inline assembly string
// <http://en.wikipedia.org/wiki/X86_assembly_language#Syntax>
sealed trait Dialect
case object Dialect {
  case object ATTDialect extends Dialect
  case object IntelDialect extends Dialect
}

// <http://llvm.org/docs/LangRef.html#inline-assembler-expressions>
// to be used through 'LLVM.AST.Operand.CallableOperand' with a
// 'LLVM.AST.Instruction.Call' instruction
// sealed trait InlineAssembly
case class InlineAssembly(
    t: Type,
    assembly: String,
    constraints: String,
    hasSideEffects: Boolean,
    alignStack: Boolean,
    dialect: Dialect
) extends CallableOperand
