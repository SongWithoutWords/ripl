// 'Global's - top-level values in 'Module's - and supporting structures.
package ripl.llvm.ast

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
  ) extends Global

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
  ) extends Global

// <http://llvm.org/docs/LangRef.html#functions>
case class Function(
    linkage: Linkage = Linkage.External,
    visibility: Visibility = Visibility.Default,
    dllStorageClass: Option[StorageClass] = None,
    callingConvention: CallingConvention = CallingConvention.C,
    returnAttributes: List[ParameterAttribute] = Nil,
    returnType: Type = VoidType,
    name: Name = Name(""),
    parameters: Parameters = Parameters(Nil, false),
    functionAttributes: List[Either[GroupID, FunctionAttribute]] = Nil,
    section: Option[String] = None,
    comdat: Option[String] = None,
    alignment: Int = 0,
    garbageCollectorName: Option[String] = None,
    prefix: Option[Constant] = None,
    basicBlocks: List[BasicBlock] = Nil,
    personalityFunction: Option[Constant] = None
  ) extends Global

case class Parameters(params: List[Parameter], isVarArg: Boolean = false)

// 'Parameter's for 'Function's
case class Parameter(
    t: Type,
    name: Name,
    attributes: List[ParameterAttribute] = Nil
  )

// <http://llvm.org/doxygen/classllvm_1_1BasicBlock.html>
// LLVM code in a function is a sequence of 'BasicBlock's each with a label,
// some instructions, and a terminator.
case class BasicBlock(
    name: Name,
    instructions: List[Named[Instruction]],
    terminator: Named[Terminator]
  )

sealed trait UnnamedAddr
case object LocalAddr  extends UnnamedAddr
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
