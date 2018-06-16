// This module and descendants define AST data types to represent LLVM code.
// Note that these types are designed for fidelity rather than convenience - if the truth
// of what LLVM supports is less than pretty, so be it.

package ripl.llvm.ast

// module LLVM.AST (
//   Module(..), defaultModule,
//   Definition(..),
//   Global(GlobalVariable, GlobalAlias, Function),
//         globalVariableDefaults,
//         globalAliasDefaults,
//         functionDefaults,
//   UnnamedAddr(..),
//   Parameter(..),
//   BasicBlock(..),
//   module LLVM.AST.Instruction,
//   module LLVM.AST.Name,
//   module LLVM.AST.Operand,
//   module LLVM.AST.Type
//   ) where

// import LLVM.Prelude

// import LLVM.AST.Name
// import LLVM.AST.Type (Type(..), FloatingPointType(..))
// import LLVM.AST.Global
// import LLVM.AST.Operand
// import LLVM.AST.Instruction
// import LLVM.AST.DataLayout
// import qualified LLVM.AST.Attribute as A
// import qualified LLVM.AST.COMDAT as COMDAT

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
// defaultModule: Module
// defaultModule =
//   Module {
//     moduleName = "<string>",
//     moduleSourceFileName = "<string>",
//     moduleDataLayout = Nothing,
//     moduleTargetTriple = Nothing,
//     moduleDefinitions = []
//   }
