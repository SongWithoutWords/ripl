// {-# LANGUAGE FlexibleContexts #-}

package ripl.llvm.irBuilder

import ripl.llvm.ast._

// module LLVM.IRBuilder.Instruction where

// import Prelude hiding (and, or, pred)

// import Data.Word

// import LLVM.AST hiding (args, dests)
// import LLVM.AST.Type as AST
// import LLVM.AST.Typed
// import LLVM.AST.ParameterAttribute
// import qualified LLVM.AST as AST
// import qualified LLVM.AST.CallingConvention as CC
// import qualified LLVM.AST.Constant as C
// import qualified LLVM.AST.IntegerPredicate as IP
// import qualified LLVM.AST.FloatingPointPredicate as FP

// import LLVM.IRBuilder.Monad

import ripl.llvm.irBuilder.IRBuilderAliases._

case object IRBuilderInstruction {

  def fadd(a: Operand, b: Operand): IRBuilder[Operand] =
    emitInstr(
      typeOf(a),
      Instruction
        .FAdd(FastMathFlags.NoFastMathFlags, a, b, InstructionMetadata())
    )

  def fsub(a: Operand, b: Operand): IRBuilder[Operand] =
    emitInstr(
      typeOf(a),
      Instruction
        .FSub(FastMathFlags.NoFastMathFlags, a, b, InstructionMetadata())
    )

  def fmul(a: Operand, b: Operand): IRBuilder[Operand] =
    emitInstr(
      typeOf(a),
      Instruction
        .FMul(FastMathFlags.NoFastMathFlags, a, b, InstructionMetadata())
    )

  def fdiv(a: Operand, b: Operand): IRBuilder[Operand] =
    emitInstr(
      typeOf(a),
      Instruction
        .FDiv(FastMathFlags.NoFastMathFlags, a, b, InstructionMetadata())
    )

  def frem(a: Operand, b: Operand): IRBuilder[Operand] =
    emitInstr(
      typeOf(a),
      Instruction
        .FRem(FastMathFlags.NoFastMathFlags, a, b, InstructionMetadata())
    )

  def add(a: Operand, b: Operand): IRBuilder[Operand] =
    emitInstr(
      typeOf(a),
      Instruction.Add(false, false, a, b, InstructionMetadata())
    )

  def sub(a: Operand, b: Operand): IRBuilder[Operand] =
    emitInstr(
      typeOf(a),
      Instruction.Sub(false, false, a, b, InstructionMetadata())
    )

  def mul(a: Operand, b: Operand): IRBuilder[Operand] =
    emitInstr(
      typeOf(a),
      Instruction.Mul(false, false, a, b, InstructionMetadata())
    )

  def udiv(a: Operand, b: Operand): IRBuilder[Operand] =
    emitInstr(
      typeOf(a),
      Instruction.UDiv(false, a, b, InstructionMetadata())
    )

  def sdiv(a: Operand, b: Operand): IRBuilder[Operand] =
    emitInstr(
      typeOf(a),
      Instruction.SDiv(false, a, b, InstructionMetadata())
    )

  def urem(a: Operand, b: Operand): IRBuilder[Operand] =
    emitInstr(
      typeOf(a),
      Instruction.URem(a, b, InstructionMetadata())
    )

  def srem(a: Operand, b: Operand): IRBuilder[Operand] =
    emitInstr(
      typeOf(a),
      Instruction.SRem(a, b, InstructionMetadata())
    )

  def shl(a: Operand, b: Operand): IRBuilder[Operand] =
    emitInstr(
      typeOf(a),
      Instruction.Shl(false, false, a, b, InstructionMetadata())
    )

  def lshr(a: Operand, b: Operand): IRBuilder[Operand] =
    emitInstr(
      typeOf(a),
      Instruction.LShr(false, a, b, InstructionMetadata())
    )

  def ashr(a: Operand, b: Operand): IRBuilder[Operand] =
    emitInstr(
      typeOf(a),
      Instruction.AShr(true, a, b, InstructionMetadata())
    )

  def and(a: Operand, b: Operand): IRBuilder[Operand] =
    emitInstr(
      typeOf(a),
      Instruction.And(a, b, InstructionMetadata())
    )

  def or(a: Operand, b: Operand): IRBuilder[Operand] =
    emitInstr(
      typeOf(a),
      Instruction.Or(a, b, InstructionMetadata())
    )

  def xor(a: Operand, b: Operand): IRBuilder[Operand] =
    emitInstr(
      typeOf(a),
      Instruction.Xor(a, b, InstructionMetadata())
    )

  def alloca(t: Type, count: Option[Operand], align: Int): IRBuilder[Operand] =
    emitInstr(
      TypeAliases.ptr(t),
      Instruction.Alloca(t, count, align, InstructionMetadata())
    )

  def load(addr: Operand, align: Int): IRBuilder[Operand] = {
    val typ = typeOf(addr) match {
      case PointerType(t, _) => t
      case _ =>
        throw new Exception("Cannot load non-pointer (Malformed AST).")
    }
    emitInstr(
      typ,
      Instruction.Load(false, addr, None, align, InstructionMetadata())
    )
  }

  def store(addr: Operand, align: Int, value: Operand): IRBuilder[Unit] =
    emitInstrVoid(
      Instruction.Store(false, addr, value, None, align, InstructionMetadata())
    )

  def gep(addr: Operand, offsets: List[Operand]): IRBuilder[Operand] = {

    // TODO: This is not quite as forgiving as the equivalent in the llvm code-base,
    // because theirs allows the use of non-const indices within arrays

    val constantOperands: List[Constant] = offsets.collect {
      case ConstantOperand(c) => c;
      case _                  => throw new Exception()
    }
    val gepType = getElementType(typeOf(addr))
    emitInstr(
      gepType,
      Instruction.GetElementPtr(false, addr, offsets, InstructionMetadata())
    )
  }

  def trunc(a: Operand, to: Type): IRBuilder[Operand] =
    emitInstr(to, Instruction.Trunc(a, to, InstructionMetadata()))

  def fpTrunc(a: Operand, to: Type): IRBuilder[Operand] =
    emitInstr(to, Instruction.FPTrunc(a, to, InstructionMetadata()))

  def zext(a: Operand, to: Type): IRBuilder[Operand] =
    emitInstr(to, Instruction.ZExt(a, to, InstructionMetadata()))

  def sext(a: Operand, to: Type): IRBuilder[Operand] =
    emitInstr(to, Instruction.SExt(a, to, InstructionMetadata()))

  def fptoui(a: Operand, to: Type): IRBuilder[Operand] =
    emitInstr(to, Instruction.FPToUI(a, to, InstructionMetadata()))

  def fptosi(a: Operand, to: Type): IRBuilder[Operand] =
    emitInstr(to, Instruction.FPToSI(a, to, InstructionMetadata()))

  def fpext(a: Operand, to: Type): IRBuilder[Operand] =
    emitInstr(to, Instruction.FPExt(a, to, InstructionMetadata()))

  def uitofp(a: Operand, to: Type): IRBuilder[Operand] =
    emitInstr(to, Instruction.UIToFP(a, to, InstructionMetadata()))

  def sitofp(a: Operand, to: Type): IRBuilder[Operand] =
    emitInstr(to, Instruction.SIToFP(a, to, InstructionMetadata()))

  def ptrtoint(a: Operand, to: Type): IRBuilder[Operand] =
    emitInstr(to, Instruction.PtrToInt(a, to, InstructionMetadata()))

  def inttoptr(a: Operand, to: Type): IRBuilder[Operand] =
    emitInstr(to, Instruction.IntToPtr(a, to, InstructionMetadata()))

  def bitcast(a: Operand, to: Type): IRBuilder[Operand] =
    emitInstr(to, Instruction.BitCast(a, to, InstructionMetadata()))

  def extractElement(vector: Operand, index: Operand): IRBuilder[Operand] =
    emitInstr(
      typeOf(vector) match {
        case VectorType(_, elementType) =>
          elementType;
        case _ => throw new Exception("Extract element expects vector type")
      },
      Instruction.ExtractElement(vector, index, InstructionMetadata())
    )

  def insertElement(
      vector: Operand,
      element: Operand,
      index: Operand
  ): IRBuilder[Operand] =
    emitInstr(
      typeOf(vector),
      Instruction.InsertElement(vector, element, index, InstructionMetadata())
    )

  def shuffleVector(
      vectorA: Operand,
      vectorB: Operand,
      mask: Constant
  ): IRBuilder[Operand] =
    // TODO: This return type is WRONG
    emitInstr(
      typeOf(vectorA),
      Instruction.ShuffleVector(vectorA, vectorB, mask, InstructionMetadata())
    )

  def extractValue(aggregate: Operand, indices: List[Int]): IRBuilder[Operand] =
    emitInstr(
      extractValueType(indices, typeOf(aggregate)),
      Instruction.ExtractValue(aggregate, indices, InstructionMetadata())
    )

  def insertValue(
      aggregate: Operand,
      element: Operand,
      indices: List[Int]
  ): IRBuilder[Operand] =
    emitInstr(
      typeOf(aggregate),
      Instruction
        .InsertValue(aggregate, element, indices, InstructionMetadata())
    )

  def icmp(pred: IntegerPredicate, a: Operand, b: Operand): IRBuilder[Operand] =
    emitInstr(
      TypeAliases.i1,
      Instruction.ICmp(pred, a, b, InstructionMetadata())
    )

  def fcmp(
      pred: FloatingPointPredicate,
      a: Operand,
      b: Operand
  ): IRBuilder[Operand] =
    emitInstr(
      TypeAliases.i1,
      Instruction.FCmp(pred, a, b, InstructionMetadata())
    )

  def phi(incoming: List[(Operand, Name)]): IRBuilder[Operand] = {
    val ty = typeOf(incoming.head._1)
    emitInstr(ty, Instruction.Phi(ty, incoming, InstructionMetadata()))
  }

  def defaultCallInstruction(
      f: Operand,
      args: List[Argument]
  ) = Instruction.Call(
    None, // tailCallKind
    CallingConvention.Fast,
    Nil, // returnAttributes
    f, // callableOperand
    args,
    Nil, // functionAttributes
    InstructionMetadata() // metadata
  )

  def call(
      f: Operand,
      args: List[Argument]
  ): IRBuilder[Option[Operand]] = {
    val instr = defaultCallInstruction(f, args)
    typeOf(f) match {
      case FunctionType(VoidType, _, _) =>
        for { _ <- emitInstrVoid(instr) } yield (None)
      case FunctionType(resType, _, _) =>
        emitInstr(resType, instr).map(Some(_))
    }
  }

  def select(cond: Operand, t: Operand, f: Operand): IRBuilder[Operand] =
    emitInstr(typeOf(t), Instruction.Select(cond, t, f, InstructionMetadata()))

  def retVoid(): IRBuilder[Unit] =
    emitTerm(Terminator.Ret(None, InstructionMetadata()))

  def ret(ret: Operand): IRBuilder[Unit] =
    emitTerm(Terminator.Ret(Some(ret), InstructionMetadata()))

  def condBr(cond: Operand, trueDest: Name, falseDest: Name): IRBuilder[Unit] =
    emitTerm(
      Terminator.CondBr(cond, trueDest, falseDest, InstructionMetadata())
    )

  def br(target: Name): IRBuilder[Unit] =
    emitTerm(Terminator.Br(target, InstructionMetadata()))

  def switch(op: Operand, default: Name, destinations: List[(Constant, Name)]) =
    emitTerm(
      Terminator.Switch(op, default, destinations, InstructionMetadata())
    )

  def unreachable() =
    emitTerm(Terminator.Unreachable(InstructionMetadata()))
}
