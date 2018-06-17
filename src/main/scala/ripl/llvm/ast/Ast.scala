// This module and descendants define AST data types to represent LLVM code.
// Note that these types are designed for fidelity rather than convenience - if the truth
// of what LLVM supports is less than pretty, so be it.

package ripl.llvm.ast

// Any thing which can be at the top level of a 'Module'
sealed trait Definition
case class GlobalDefinition(g: Global) extends Definition
case class TypeDefinition(name: Name, t: Option[Type]) extends Definition
case class MetadataNodeDefinition(
    nodeID: MetadataNodeID,
    metaData: List[Option[Metadata]]
) extends Definition
case class NamedMetadataDefinition(name: String, metaData: List[MetadataNodeID])
    extends Definition
case class ModuleInlineAssembly(asm: String) extends Definition
case class FunctionAttributes(
    groupId: GroupID,
    functionAttributs: List[FunctionAttribute]
) extends Definition
case class COMDAT(string: String, selectionKind: SelectionKind)
    extends Definition


// <http://llvm.org/docs/LangRef.html#module-structure>
case class Module(
    moduleName: String,
    moduleSourceFileName: String,
    // a 'DataLayout', if specified, must match that of the eventual code generator
    moduleDataLayout: Option[DataLayout],
    moduleTargetTriple: Option[String],
    moduleDefinitions: List[Definition]
)

// helper for making 'Module's
case object Module {
  val default = Module("<string>", "<string>", None, None, Nil)
}

