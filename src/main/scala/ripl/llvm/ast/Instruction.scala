// LLVM instructions
// <http://llvm.org/docs/LangRef.html#instruction-reference>

package ripl.llvm.ast

case class NonEmpty[A](head: A, tail: List[A]) {
  def toList(): List[A] = head :: tail
}

// <http://llvm.org/docs/LangRef.html#metadata-nodes-and-metadata-strings>
// Metadata can be attached to an instruction
// object InstructionAliases {
// An 'Atomicity' describes constraints on the visibility of effects of an atomic instruction
//   type Atomicity = (SynchronizationScope, MemoryOrdering)

//   type InstructionMetadata = List[(String, MetadataNode)]
// }
// import InstructionAliases._

case class Argument(op: Operand, attrs: List[ParameterAttribute])
case class Atomicity(scope: SynchronizationScope, order: MemoryOrdering)
case object InstructionMetadata {
  def apply(data: (String, MetadataNode)*): InstructionMetadata =
    InstructionMetadata(data.toList)
}
case class InstructionMetadata(data: List[(String, MetadataNode)])

// <http://llvm.org/docs/LangRef.html#terminators>
sealed trait Terminator
case object Terminator {
  case class Ret(returnOperand: Option[Operand], metadata: InstructionMetadata)
      extends Terminator
  case class CondBr(
      condition: Operand,
      trueDest: Name,
      falseDest: Name,
      metadata: InstructionMetadata
    ) extends Terminator
  case class Br(dest: Name, metadata: InstructionMetadata) extends Terminator
  case class Switch(
      operand0: Operand,
      defaultDest: Name,
      dests: List[(Constant, Name)],
      metadata: InstructionMetadata
    ) extends Terminator
  case class IndirectBr(
      operand0: Operand,
      possibleDests: List[Name],
      metadata: InstructionMetadata
    ) extends Terminator
  case class Invoke(
      callingConvention: CallingConvention,
      returnAttributes: List[ParameterAttribute],
      function: CallableOperand,
      arguments: List[Argument],
      functionAttributes: List[Either[GroupID, FunctionAttribute]],
      returnDest: Name,
      exceptionDest: Name,
      metadata: InstructionMetadata
    ) extends Terminator
  case class Resume(operand0: Operand, metadata: InstructionMetadata)
      extends Terminator
  case class Unreachable(metadata: InstructionMetadata) extends Terminator
  case class CleanupRet(
      cleanupPad: Operand,
      unwindDest: Option[Name],
      metadata: InstructionMetadata
    ) extends Terminator
  case class CatchRet(
      catchPad: Operand,
      successor: Name,
      metadata: InstructionMetadata
    ) extends Terminator
  case class CatchSwitch(
      parentPad: Operand,
      catchHandlers: NonEmpty[Name],
      defaultUnwindDest: Option[Name],
      metadata: InstructionMetadata
    ) extends Terminator
}

// <http://llvm.org/docs/LangRef.html#fast-math-flags>
sealed trait FastMathFlags
case object FastMathFlags {
  case object NoFastMathFlags extends FastMathFlags
  case object UnsafeAlgebra   extends FastMathFlags
  case class Flags(
      noNaNs: Boolean,
      noInfs: Boolean,
      noSignedZeros: Boolean,
      allowReciprocal: Boolean
    ) extends FastMathFlags
}

// <http://llvm.org/docs/LangRef.html#atomic-memory-ordering-constraints>
// <http://llvm.org/docs/Atomics.html>
sealed trait MemoryOrdering
case object Unordered              extends MemoryOrdering
case object Monotonic              extends MemoryOrdering
case object Acquire                extends MemoryOrdering
case object Release                extends MemoryOrdering
case object AcquireRelease         extends MemoryOrdering
case object SequentiallyConsistent extends MemoryOrdering

// <http://llvm.org/docs/LangRef.html#singlethread>
sealed trait SynchronizationScope
case object SingleThread extends SynchronizationScope
case object System       extends SynchronizationScope

// For the redoubtably complex 'LandingPad' instruction
sealed trait LandingPadClause
case class Catch(constant: Constant)  extends LandingPadClause
case class Filter(constant: Constant) extends LandingPadClause

// For the call instruction
// <http://llvm.org/docs/LangRef.html#call-instruction>
sealed trait TailCallKind
case object Tail     extends TailCallKind
case object MustTail extends TailCallKind
case object NoTail   extends TailCallKind

// non-terminator instructions:
// <http://llvm.org/docs/LangRef.html#binaryops>
// <http://llvm.org/docs/LangRef.html#bitwiseops>
// <http://llvm.org/docs/LangRef.html#memoryops>
// <http://llvm.org/docs/LangRef.html#otherops>
sealed trait Instruction
case object Instruction {
  case class Add(
      nsw: Boolean,
      nuw: Boolean,
      operand0: Operand,
      operand1: Operand,
      metadata: InstructionMetadata
    ) extends Instruction
  case class FAdd(
      fastMathFlags: FastMathFlags,
      operand0: Operand,
      operand1: Operand,
      metadata: InstructionMetadata
    ) extends Instruction
  case class Sub(
      nsw: Boolean,
      nuw: Boolean,
      operand0: Operand,
      operand1: Operand,
      metadata: InstructionMetadata
    ) extends Instruction
  case class FSub(
      fastMathFlags: FastMathFlags,
      operand0: Operand,
      operand1: Operand,
      metadata: InstructionMetadata
    ) extends Instruction
  case class Mul(
      nsw: Boolean,
      nuw: Boolean,
      operand0: Operand,
      operand1: Operand,
      metadata: InstructionMetadata
    ) extends Instruction
  case class FMul(
      fastMathFlags: FastMathFlags,
      operand0: Operand,
      operand1: Operand,
      metadata: InstructionMetadata
    ) extends Instruction
  case class UDiv(
      exact: Boolean,
      operand0: Operand,
      operand1: Operand,
      metadata: InstructionMetadata
    ) extends Instruction
  case class SDiv(
      exact: Boolean,
      operand0: Operand,
      operand1: Operand,
      metadata: InstructionMetadata
    ) extends Instruction
  case class FDiv(
      fastMathFlags: FastMathFlags,
      operand0: Operand,
      operand1: Operand,
      metadata: InstructionMetadata
    ) extends Instruction
  case class URem(
      operand0: Operand,
      operand1: Operand,
      metadata: InstructionMetadata
    ) extends Instruction
  case class SRem(
      operand0: Operand,
      operand1: Operand,
      metadata: InstructionMetadata
    ) extends Instruction
  case class FRem(
      fastMathFlags: FastMathFlags,
      operand0: Operand,
      operand1: Operand,
      metadata: InstructionMetadata
    ) extends Instruction
  case class Shl(
      nsw: Boolean,
      nuw: Boolean,
      operand0: Operand,
      operand1: Operand,
      metadata: InstructionMetadata
    ) extends Instruction
  case class LShr(
      exact: Boolean,
      operand0: Operand,
      operand1: Operand,
      metadata: InstructionMetadata
    ) extends Instruction
  case class AShr(
      exact: Boolean,
      operand0: Operand,
      operand1: Operand,
      metadata: InstructionMetadata
    ) extends Instruction
  case class And(
      operand0: Operand,
      operand1: Operand,
      metadata: InstructionMetadata
    ) extends Instruction
  case class Or(
      operand0: Operand,
      operand1: Operand,
      metadata: InstructionMetadata
    ) extends Instruction
  case class Xor(
      operand0: Operand,
      operand1: Operand,
      metadata: InstructionMetadata
    ) extends Instruction
  case class Alloca(
      allocatedType: Type,
      numElements: Option[Operand],
      alignment: Int,
      metadata: InstructionMetadata
    ) extends Instruction
  case class Load(
      volatile: Boolean,
      address: Operand,
      maybeAtomicity: Option[Atomicity],
      alignment: Int,
      metadata: InstructionMetadata
    ) extends Instruction
  case class Store(
      volatile: Boolean,
      address: Operand,
      value: Operand,
      maybeAtomicity: Option[Atomicity],
      alignment: Int,
      metadata: InstructionMetadata
    ) extends Instruction
  case class GetElementPtr(
      inBounds: Boolean,
      address: Operand,
      indices: List[Operand],
      metadata: InstructionMetadata
    ) extends Instruction
  case class Fence(atomicity: Atomicity, metadata: InstructionMetadata)
      extends Instruction
  case class CmpXchg(
      volatile: Boolean,
      address: Operand,
      expected: Operand,
      replacement: Operand,
      atomicity: Atomicity,
      failureMemoryOrdering: MemoryOrdering,
      metadata: InstructionMetadata
    ) extends Instruction
  case class AtomicRMW(
      volatile: Boolean,
      rmwOperation: RMWOperation,
      address: Operand,
      value: Operand,
      atomicity: Atomicity,
      metadata: InstructionMetadata
    ) extends Instruction
  case class Trunc(operand0: Operand, t: Type, metadata: InstructionMetadata)
      extends Instruction
  case class ZExt(operand0: Operand, t: Type, metadata: InstructionMetadata)
      extends Instruction
  case class SExt(operand0: Operand, t: Type, metadata: InstructionMetadata)
      extends Instruction
  case class FPToUI(operand0: Operand, t: Type, metadata: InstructionMetadata)
      extends Instruction
  case class FPToSI(operand0: Operand, t: Type, metadata: InstructionMetadata)
      extends Instruction
  case class UIToFP(operand0: Operand, t: Type, metadata: InstructionMetadata)
      extends Instruction
  case class SIToFP(operand0: Operand, t: Type, metadata: InstructionMetadata)
      extends Instruction
  case class FPTrunc(operand0: Operand, t: Type, metadata: InstructionMetadata)
      extends Instruction
  case class FPExt(operand0: Operand, t: Type, metadata: InstructionMetadata)
      extends Instruction
  case class PtrToInt(operand0: Operand, t: Type, metadata: InstructionMetadata)
      extends Instruction
  case class IntToPtr(operand0: Operand, t: Type, metadata: InstructionMetadata)
      extends Instruction
  case class BitCast(operand0: Operand, t: Type, metadata: InstructionMetadata)
      extends Instruction
  case class AddrSpaceCast(
      operand0: Operand,
      t: Type,
      metadata: InstructionMetadata
    ) extends Instruction
  case class ICmp(
      iPredicate: IntegerPredicate,
      operand0: Operand,
      operand1: Operand,
      metadata: InstructionMetadata
    ) extends Instruction
  case class FCmp(
      fpPredicate: FloatingPointPredicate,
      operand0: Operand,
      operand1: Operand,
      metadata: InstructionMetadata
    ) extends Instruction
  case class Phi(
      t: Type,
      incomingValues: List[(Operand, Name)],
      metadata: InstructionMetadata
    ) extends Instruction
  case class Call(
      tailCallKind: Option[TailCallKind],
      callingConvention: CallingConvention,
      returnAttributes: List[ParameterAttribute],
      function: CallableOperand,
      arguments: List[Argument],
      functionAttributes: List[Either[GroupID, FunctionAttribute]],
      metadata: InstructionMetadata
    ) extends Instruction
  case class Select(
      condition: Operand,
      trueValue: Operand,
      falseValue: Operand,
      metadata: InstructionMetadata
    ) extends Instruction
  case class VAArg(argList: Operand, t: Type, metadata: InstructionMetadata)
      extends Instruction
  case class ExtractElement(
      vector: Operand,
      index: Operand,
      metadata: InstructionMetadata
    ) extends Instruction
  case class InsertElement(
      vector: Operand,
      element: Operand,
      index: Operand,
      metadata: InstructionMetadata
    ) extends Instruction
  case class ShuffleVector(
      operand0: Operand,
      operand1: Operand,
      mask: Constant,
      metadata: InstructionMetadata
    ) extends Instruction
  case class ExtractValue(
      aggregate: Operand,
      indices: List[Int],
      metadata: InstructionMetadata
    ) extends Instruction
  case class InsertValue(
      aggregate: Operand,
      element: Operand,
      indices: List[Int],
      metadata: InstructionMetadata
    ) extends Instruction
  case class LandingPad(
      t: Type,
      cleanup: Boolean,
      clauses: List[LandingPadClause],
      metadata: InstructionMetadata
    ) extends Instruction
  case class CatchPad(
      catchSwitch: Operand,
      args: List[Operand],
      metadata: InstructionMetadata
    ) extends Instruction
  case class CleanupPad(
      parentPad: Operand,
      args: List[Operand],
      metadata: InstructionMetadata
    ) extends Instruction
}

// Instances of instructions may be given a name, allowing their results to be referenced as 'Operand's.
// Sometimes instructions - e.g. a call to a function returning void - don't need names.
sealed trait Named[A] {
  def map[B](f: A => B): Named[B]
}
case class :=[A](name: Name, a: A) extends Named[A] {
  def map[B](f: A => B) = name := f(a)
}
case class Do[A](a: A) extends Named[A] {
  def map[B](f: A => B) = Do(f(a))
}
