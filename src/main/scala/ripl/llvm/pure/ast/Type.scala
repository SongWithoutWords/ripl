// A representation of an LLVM type
package ripl.llvm.pure.ast

// LLVM supports some special formats floating point format. This type is to distinguish those format. Also see  <http://llvm.org/docs/LangRef.html#floating-point-types>

sealed trait FloatingPointType extends Type
case object HalfFP extends FloatingPointType // ^ 16-bit floating point value
case object FloatFP extends FloatingPointType // ^ 32-bit floating point value
case object DoubleFP extends FloatingPointType // ^ 64-bit floating point value
case object FP128FP extends FloatingPointType // ^ 128-bit floating point value (112-bit mantissa)
case object X86_FP80FP extends FloatingPointType // ^ 80-bit floating point value (X87)
case object PPC_FP128FP extends FloatingPointType // ^ 128-bit floating point value (two 64-bits)

// <http://llvm.org/docs/LangRef.html#type-system>
sealed trait Type
// <http://llvm.org/docs/LangRef.html#void-type>
case object VoidType extends Type
// <http://llvm.org/docs/LangRef.html#integer-type>
case class IntegerType(typeBits: scala.Int) extends Type
// <http://llvm.org/docs/LangRef.html#pointer-type>
case class PointerType(pointerReferent: Type, pointerAddrSpace: AddrSpace)
    extends Type
// <http://llvm.org/docs/LangRef.html#function-type>
case class FunctionType(
    resultType: Type,
    argumentTypes: List[Type],
    isVarArg: Boolean = false
) extends Type
// <http://llvm.org/docs/LangRef.html#vector-type>
case class VectorType(nVectorElements: Int, elementType: Type) extends Type
// <http://llvm.org/docs/LangRef.html#structure-type>
case class StructureType(isPacked: Boolean, elementTypes: List[Type])
    extends Type
// <http://llvm.org/docs/LangRef.html#array-type>
case class ArrayType(nArrayElements: Long, elementType: Type) extends Type
// <http://llvm.org/docs/LangRef.html#opaque-structure-types>
case class NamedTypeReference(name: Name) extends Type
// <http://llvm.org/docs/LangRef.html#metadata-type>
case object MetadataType // only to be used as a parameter type for a few intrinsics
// <http://llvm.org/docs/LangRef.html#token-type>
case object LabelType // only to be used as the type of block names
// <http://llvm.org/docs/LangRef.html#label-type>
case object TokenType
// deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

// // An abbreviation for 'VoidType'
// void: Type
// void = VoidType

// // An abbreviation for 'IntegerType' 1
// i1: Type
// i1 = IntegerType 1

// // An abbreviation for 'IntegerType' 8
// i8: Type
// i8 = IntegerType 8

// // An abbreviation for 'IntegerType' 16
// i16: Type
// i16 = IntegerType 16

// // An abbreviation for 'IntegerType' 32
// i32: Type
// i32 = IntegerType 32

// // An abbreviation for 'IntegerType' 64
// i64: Type
// i64 = IntegerType 64

// // An abbreviation for 'IntegerType' 128
// i128: Type
// i128 = IntegerType 128

// // An abbreviation for 'PointerType' t ('AddrSpace' 0)
// ptr: Type -> Type
// ptr t = PointerType t (AddrSpace 0)

// // An abbreviation for 'FloatingPointType' 'HalfFP'
// half: Type
// half = FloatingPointType HalfFP

// // An abbreviation for 'FloatingPointType' 'FloatFP'
// float: Type
// float = FloatingPointType FloatFP

// // An abbreviation for 'FloatingPointType' 'DoubleFP'
// double: Type
// double = FloatingPointType DoubleFP

// // An abbreviation for 'FloatingPointType' 'FP128FP'
// fp128: Type
// fp128 = FloatingPointType FP128FP

// // An abbreviation for 'FloatingPointType' 'X86_FP80FP'
// x86_fp80: Type
// x86_fp80 = FloatingPointType X86_FP80FP

// // An abbreviation for 'FloatingPointType' 'PPC_FP128FP'
// ppc_fp128: Type
// ppc_fp128 = FloatingPointType PPC_FP128FP
