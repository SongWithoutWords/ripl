// 'Global's - top-level values in 'Module's - and supporting structures.
package ripl.llvm.pure.ast

// <http://llvm.org/doxygen/classllvm_1_1GlobalValue.html>
sealed trait Global
// <http://llvm.org/docs/LangRef.html#global-variables>
case class GlobalVariable(
    name: Name,
    linkage: Linkage,
    visibility: Visibility,
    dllStorageClass: Option[StorageClass],
    threadLocalMode: Option[ThreadLocalStorageModel],
    unnamedAddr: Option[UnnamedAddr],
    isConstant: Boolean,
    t: Type,
    addrSpace: AddrSpace,
    initializer: Option[Constant],
    section: Option[String],
    comdat: Option[String],
    alignment: Int
)
// <http://llvm.org/docs/LangRef.html#aliases>
case class GlobalAlias(
    name: Name,
    linkage: Linkage,
    visibility: Visibility,
    dllStorageClass: Option[StorageClass],
    threadLocalMode: Option[ThreadLocalStorageModel],
    unnamedAddr: Option[UnnamedAddr],
    t: Type,
    addrSpace: AddrSpace,
    aliasee: Constant
)
// <http://llvm.org/docs/LangRef.html#functions>
case class Function(
    linkage: Linkage,
    visibility: Visibility,
    dllStorageClass: Option[StorageClass],
    callingConvention: CallingConvention,
    returnAttributes: List[ParameterAttribute],
    returnType: Type,
    name: Name,
    parameters: (List[Parameter], Boolean), // ^ snd indicates varargs
    functionAttributes: List[Either[GroupID, FunctionAttribute]],
    section: Option[String],
    comdat: Option[String],
    alignment: Int,
    garbageCollectorName: Option[String],
    prefix: Option[Constant],
    basicBlocks: List[BasicBlock],
    personalityFunction: Option[Constant]
)

// 'Parameter's for 'Function's
case class Parameter(t: Type, name: Name, attributes: List[ParameterAttribute])

// <http://llvm.org/doxygen/classllvm_1_1BasicBlock.html>
// LLVM code in a function is a sequence of 'BasicBlock's each with a label,
// some instructions, and a terminator.
case class BasicBlock(
    name: Name,
    instructions: List[Named[Instruction]],
    terminator: Named[Terminator]
)

sealed trait UnnamedAddr
case object LocalAddr extends UnnamedAddr
case object GlobalAddr extends UnnamedAddr

// // helper for making 'GlobalVariable's
// globalVariableDefaults: Global
// globalVariableDefaults =
//   GlobalVariable(
//   name = error "global variable name not defined",
//   linkage = L.External,
//   visibility = V.Default,
//   dllStorageClass = Nothing,
//   threadLocalMode = Nothing,
//   addrSpace = AddrSpace 0,
//   unnamedAddr = Nothing,
//   isConstant = False,
//   t = error "global variable type not defined",
//   initializer = Nothing,
//   section = Nothing,
//   comdat = Nothing,
//   alignment = 0
//   )

// // helper for making 'GlobalAlias's
// globalAliasDefaults: Global
// globalAliasDefaults =
//   GlobalAlias(
//     name = error "global alias name not defined",
//     linkage = L.External,
//     visibility = V.Default,
//     dllStorageClass = Nothing,
//     threadLocalMode = Nothing,
//     unnamedAddr = Nothing,
//     t = error "global alias type not defined",
//     addrSpace = AddrSpace 0,
//     aliasee = error "global alias aliasee not defined"
//   )

// // helper for making 'Function's
// functionDefaults: Global
// functionDefaults =
//   Function(
//     linkage = L.External,
//     visibility = V.Default,
//     dllStorageClass = Nothing,
//     callingConvention = CC.C,
//     returnAttributes = [],
//     returnType = error "function return type not defined",
//     name = error "function name not defined",
//     parameters = ([], False),
//     functionAttributes = [],
//     section = Nothing,
//     comdat = Nothing,
//     alignment = 0,
//     garbageCollectorName = Nothing,
//     prefix = Nothing,
//     basicBlocks = [],
//     personalityFunction = Nothing
//   )
