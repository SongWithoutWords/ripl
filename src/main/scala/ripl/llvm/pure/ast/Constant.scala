// A representation of LLVM constants
package ripl.llvm.pure.ast

// import LLVM.Prelude

// import Data.Bits ((.|.), (.&.), complement, testBit, shiftL)

// import LLVM.AST.Type
// import LLVM.AST.Name
// import LLVM.AST.FloatingPointPredicate (FloatingPointPredicate)
// import LLVM.AST.IntegerPredicate (IntegerPredicate)
// import qualified LLVM.AST.Float as F

// <http://llvm.org/docs/LangRef.html#constants>

// N.B. - <http://llvm.org/docs/LangRef.html#constant-expressions>

// Although constant expressions and instructions have many similarites, there are important
// differences - so they're represented using different types in this AST. At the cost of making it
// harder to move an code back and forth between being constant and not, this approach embeds more of
// the rules of what IR is legal into the Haskell types.

sealed trait Constant
case object Constant {
  case class Integral(bitWidth: Int, value: BigInt) extends Constant
  case class Float(floatValue: SomeFloat) extends Constant
  case class Null(constantType: Type) extends Constant
  case class Struct(
      structName: Option[Name],
      isPacked: Boolean,
      memberValues: List[Constant]
  ) extends Constant
  case class Array(memberType: Type, memberValues: List[Constant])
      extends Constant
  case class Vector(memberValues: List[Constant]) extends Constant
  case class Undef(constantType: Type) extends Constant
  case class BlockAddress(blockAddressFunction: Name, blockAddressBlock: Name)
      extends Constant
  case class GlobalReference(t: Type, name: Name) extends Constant
  case object TokenNone extends Constant
  case class Add(
      nsw: Boolean,
      nuw: Boolean,
      operand0: Constant,
      operand1: Constant
  ) extends Constant
  case class FAdd(
      operand0: Constant,
      operand1: Constant
  ) extends Constant
  case class Sub(
      nsw: Boolean,
      nuw: Boolean,
      operand0: Constant,
      operand1: Constant
  ) extends Constant
  case class FSub(
      operand0: Constant,
      operand1: Constant
  ) extends Constant
  case class Mul(
      nsw: Boolean,
      nuw: Boolean,
      operand0: Constant,
      operand1: Constant
  ) extends Constant
  case class FMul(
      operand0: Constant,
      operand1: Constant
  ) extends Constant
  case class UDiv(
      exact: Boolean,
      operand0: Constant,
      operand1: Constant
  ) extends Constant
  case class SDiv(
      exact: Boolean,
      operand0: Constant,
      operand1: Constant
  ) extends Constant
  case class FDiv(
      operand0: Constant,
      operand1: Constant
  ) extends Constant
  case class URem(
      operand0: Constant,
      operand1: Constant
  ) extends Constant
  case class SRem(
      operand0: Constant,
      operand1: Constant
  ) extends Constant
  case class FRem(
      operand0: Constant,
      operand1: Constant
  ) extends Constant
  case class Shl(
      nsw: Boolean,
      nuw: Boolean,
      operand0: Constant,
      operand1: Constant
  ) extends Constant
  case class LShr(
      exact: Boolean,
      operand0: Constant,
      operand1: Constant
  ) extends Constant
  case class AShr(
      exact: Boolean,
      operand0: Constant,
      operand1: Constant
  ) extends Constant
  case class And(
      operand0: Constant,
      operand1: Constant
  ) extends Constant
  case class Or(
      operand0: Constant,
      operand1: Constant
  ) extends Constant
  case class Xor(
      operand0: Constant,
      operand1: Constant
  ) extends Constant
  case class GetElementPtr(
      inBounds: Boolean,
      address: Constant,
      indices: List[Constant]
  ) extends Constant
  case class Trunc(
      operand0: Constant,
      t: Type
  ) extends Constant
  case class ZExt(
      operand0: Constant,
      t: Type
  ) extends Constant
  case class SExt(
      operand0: Constant,
      t: Type
  ) extends Constant
  case class FPToUI(
      operand0: Constant,
      t: Type
  ) extends Constant
  case class FPToSI(
      operand0: Constant,
      t: Type
  ) extends Constant
  case class UIToFP(
      operand0: Constant,
      t: Type
  ) extends Constant
  case class SIToFP(
      operand0: Constant,
      t: Type
  ) extends Constant
  case class FPTrunc(
      operand0: Constant,
      t: Type
  ) extends Constant
  case class FPExt(
      operand0: Constant,
      t: Type
  ) extends Constant
  case class PtrToInt(
      operand0: Constant,
      t: Type
  ) extends Constant
  case class IntToPtr(
      operand0: Constant,
      t: Type
  ) extends Constant
  case class BitCast(
      operand0: Constant,
      t: Type
  ) extends Constant
  case class AddrSpaceCast(
      operand0: Constant,
      t: Type
  ) extends Constant
  case class ICmp(
      iPredicate: IntegerPredicate,
      operand0: Constant,
      operand1: Constant
  ) extends Constant
  case class FCmp(
      fpPredicate: FloatingPointPredicate,
      operand0: Constant,
      operand1: Constant
  ) extends Constant
  case class Select(
      condition: Constant,
      trueValue: Constant,
      falseValue: Constant
  ) extends Constant
  case class ExtractElement(
      vector: Constant,
      index: Constant
  ) extends Constant
  case class InsertElement(
      vector: Constant,
      element: Constant,
      index: Constant
  ) extends Constant
  case class ShuffleVector(
      operand0: Constant,
      operand1: Constant,
      mask: Constant
  ) extends Constant
  case class ExtractValue(
      aggregate: Constant,
      indices: List[Int]
  ) extends Constant
  case class InsertValue(
      aggregate: Constant,
      element: Constant,
      indices: List[Int]
  ) extends Constant
}
// Since LLVM types don't include signedness, there's ambiguity in interpreting
// an constant as an Integer. The LLVM assembly printer prints integers as signed, but
// cheats for 1-bit integers and prints them as 'true' or 'false'. That way it circuments the
// otherwise awkward fact that a twos complement 1-bit number only has the values -1 and 0.
// signedIntegerValue: Constant -> Integer
// signedIntegerValue (Int nBits' bits) =
//   let nBits = fromIntegral nBits'
//   in
//     if bits `testBit` (nBits - 1) then bits .|. (-1 `shiftL` nBits) else bits
// signedIntegerValue _ = error "signedIntegerValue is only defined for Int"

// This library's conversion from LLVM C++ objects will always produce integer constants
// as unsigned, so this function in many cases is not necessary. However, nothing's to keep
// stop direct construction of an 'Int' with a negative 'integerValue'. There's nothing in principle
// wrong with such a value - it has perfectly good low order bits like any integer, and will be used
// as such, likely producing the intended result if lowered to C++. If, however one wishes to interpret
// an 'Int' of unknown provenance as unsigned, then this function will serve.
// unsignedIntegerValue: Constant -> Integer
// unsignedIntegerValue (Int nBits bits) =
//   bits .&. (complement (-1 `shiftL` (fromIntegral nBits)))
// unsignedIntegerValue _ = error "unsignedIntegerValue is only defined for Int"
