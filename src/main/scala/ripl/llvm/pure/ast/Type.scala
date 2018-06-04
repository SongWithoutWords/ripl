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

case object TypeAliases {

  val void = VoidType

  val i1 = IntegerType(1)

  val i8 = IntegerType(8)

  val i16 = IntegerType(16)

  val i32 = IntegerType(32)

  val i64 = IntegerType(64)

  val i128 = IntegerType(128)

  def ptr(t: Type) = PointerType(t, AddrSpace(0))

  val half = HalfFP

  val float = FloatFP

  val double = DoubleFP

  val fp128 = FP128FP

  val x86_fp80 = X86_FP80FP

  val ppc_fp128 = PPC_FP128FP
}
