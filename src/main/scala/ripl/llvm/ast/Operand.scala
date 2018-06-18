// A type to represent operands to LLVM 'LLVM.AST.Instruction.Instruction's

package ripl.llvm.ast

// An 'Operand' is roughly that which is an argument to an 'LLVM.AST.Instruction.Instruction'
sealed trait Operand extends CallableOperand
// %foo
case class LocalReference(t: Type, name: Name) extends Operand
// 'Constant's include 'LLVM.AST.Constant.GlobalReference', for \@foo
case class ConstantOperand(constant: Constant) extends Operand
case class MetadataOperand(metadata: Metadata) extends Operand

// A 'MetadataNodeID' is a number for identifying a metadata node.
// Note this is different from "named metadata", which are represented with
// 'LLVM.AST.NamedMetadataDefinition'.
case class MetadataNodeID(id: Int)

// <http://llvm.org/docs/LangRef.html#metadata>
sealed trait MetadataNode
case class MetadataNodeData(data: List[Option[Metadata]]) extends MetadataNode
case class MetadataNodeReference(data: MetadataNodeID)    extends MetadataNode

// <http://llvm.org/docs/LangRef.html#metadata>
sealed trait Metadata
// ^ <http://llvm.org/docs/doxygen/html/classllvm_1_1MDNode.html>
case class MDString(string: String) extends Metadata
// ^ <http://llvm.org/docs/doxygen/html/classllvm_1_1MDNode.html>
case class MDNode(node: MetadataNode) extends Metadata
// ^ <http://llvm.org/docs/doxygen/html/classllvm_1_1ValueAsMetadata.html>
case class MDValue(operand: Operand) extends Metadata

// The 'LLVM.AST.Instruction.Call' instruction is special: the callee can be inline assembly
trait CallableOperand
