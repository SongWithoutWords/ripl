// {-# LANGUAGE FlexibleContexts #-}

package ripl.llvm.pure.IRBuilder

import ripl.llvm.pure.ast._

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

import ripl.llvm.pure.IRBuilder.IRBuilderAliases._

case object IRBuilderInstruction {

  def fadd(a: Operand, b: Operand): IRBuilder[Operand] =
    emitInstr(
      typeOf(a),
      Instruction.FAdd(FastMathFlags.NoFastMathFlags, a, b, Nil)
    )

  def fsub(a: Operand, b: Operand): IRBuilder[Operand] =
    emitInstr(
      typeOf(a),
      Instruction.FSub(FastMathFlags.NoFastMathFlags, a, b, Nil)
    )

  def fmul(a: Operand, b: Operand): IRBuilder[Operand] =
    emitInstr(
      typeOf(a),
      Instruction.FMul(FastMathFlags.NoFastMathFlags, a, b, Nil)
    )

  def fdiv(a: Operand, b: Operand): IRBuilder[Operand] =
    emitInstr(
      typeOf(a),
      Instruction.FDiv(FastMathFlags.NoFastMathFlags, a, b, Nil)
    )

  def frem(a: Operand, b: Operand): IRBuilder[Operand] =
    emitInstr(
      typeOf(a),
      Instruction.FRem(FastMathFlags.NoFastMathFlags, a, b, Nil)
    )

  def add(a: Operand, b: Operand): IRBuilder[Operand] =
    emitInstr(
      typeOf(a),
      Instruction.Add(false, false, a, b, Nil)
    )

  def sub(a: Operand, b: Operand): IRBuilder[Operand] =
    emitInstr(
      typeOf(a),
      Instruction.Sub(false, false, a, b, Nil)
    )

  def mul(a: Operand, b: Operand): IRBuilder[Operand] =
    emitInstr(
      typeOf(a),
      Instruction.Mul(false, false, a, b, Nil)
    )

  def udiv(a: Operand, b: Operand): IRBuilder[Operand] =
    emitInstr(
      typeOf(a),
      Instruction.UDiv(false, a, b, Nil)
    )

  def sdiv(a: Operand, b: Operand): IRBuilder[Operand] =
    emitInstr(
      typeOf(a),
      Instruction.SDiv(false, a, b, Nil)
    )

  def urem(a: Operand, b: Operand): IRBuilder[Operand] =
    emitInstr(
      typeOf(a),
      Instruction.URem(a, b, Nil)
    )

  def srem(a: Operand, b: Operand): IRBuilder[Operand] =
    emitInstr(
      typeOf(a),
      Instruction.SRem(a, b, Nil)
    )

  def shl(a: Operand, b: Operand): IRBuilder[Operand] =
    emitInstr(
      typeOf(a),
      Instruction.Shl(false, false, a, b, Nil)
    )

  def lshr(a: Operand, b: Operand): IRBuilder[Operand] =
    emitInstr(
      typeOf(a),
      Instruction.LShr(false, a, b, Nil)
    )

  def ashr(a: Operand, b: Operand): IRBuilder[Operand] =
    emitInstr(
      typeOf(a),
      Instruction.AShr(true, a, b, Nil)
    )

  def and(a: Operand, b: Operand): IRBuilder[Operand] =
    emitInstr(
      typeOf(a),
      Instruction.And(a, b, Nil)
    )

  def or(a: Operand, b: Operand): IRBuilder[Operand] =
    emitInstr(
      typeOf(a),
      Instruction.Or(a, b, Nil)
    )

  def xor(a: Operand, b: Operand): IRBuilder[Operand] =
    emitInstr(
      typeOf(a),
      Instruction.Xor(a, b, Nil)
    )

  def alloca(t: Type, count: Option[Operand], align: Int): IRBuilder[Operand] =
    emitInstr(
      TypeAliases.ptr(t),
      Instruction.Alloca(t, count, align, Nil)
    )

  def load(addr: Operand, align: Int): IRBuilder[Operand] = {
    val typ = typeOf(addr) match {
      case PointerType(t, _) => t
      case _ =>
        throw new Exception("Cannot load non-pointer (Malformed AST).")
    }
    emitInstr(typ, Instruction.Load(false, addr, None, align, Nil))
  }

  def store(addr: Operand, align: Int, value: Operand): IRBuilder[Unit] =
    emitInstrVoid(Instruction.Store(false, addr, value, None, align, Nil))

  def gep(addr: Operand, offsets: List[Operand]): IRBuilder[Operand] = {

    // TODO: This is not quite as forgiving as the equivalent in the llvm code-base,
    // because theirs allows the use of non-const indices within arrays

    val constantOperands: List[Constant] = offsets.collect {
      case ConstantOperand(c) => c;
      case _                  => throw new Exception()
    }
    val gepType = getElementType(typeOf(addr))
    emitInstr(gepType, Instruction.GetElementPtr(false, addr, offsets, Nil))
  }

  def trunc(a: Operand, to: Type): IRBuilder[Operand] =
    emitInstr(to, Instruction.Trunc(a, to, Nil))

  def fpTrunc(a: Operand, to: Type): IRBuilder[Operand] =
    emitInstr(to, Instruction.FPTrunc(a, to, Nil))

  def zext(a: Operand, to: Type): IRBuilder[Operand] =
    emitInstr(to, Instruction.ZExt(a, to, Nil))

  def sext(a: Operand, to: Type): IRBuilder[Operand] =
    emitInstr(to, Instruction.SExt(a, to, Nil))

  def fptoui(a: Operand, to: Type): IRBuilder[Operand] =
    emitInstr(to, Instruction.FPToUI(a, to, Nil))

  def fptosi(a: Operand, to: Type): IRBuilder[Operand] =
    emitInstr(to, Instruction.FPToSI(a, to, Nil))

  def fpext(a: Operand, to: Type): IRBuilder[Operand] =
    emitInstr(to, Instruction.FPExt(a, to, Nil))

  def uitofp(a: Operand, to: Type): IRBuilder[Operand] =
    emitInstr(to, Instruction.UIToFP(a, to, Nil))

  def sitofp(a: Operand, to: Type): IRBuilder[Operand] =
    emitInstr(to, Instruction.SIToFP(a, to, Nil))

  def ptrtoint(a: Operand, to: Type): IRBuilder[Operand] =
    emitInstr(to, Instruction.PtrToInt(a, to, Nil))

  def inttoptr(a: Operand, to: Type): IRBuilder[Operand] =
    emitInstr(to, Instruction.IntToPtr(a, to, Nil))

  def bitcast(a: Operand, to: Type): IRBuilder[Operand] =
    emitInstr(to, Instruction.BitCast(a, to, Nil))

  def extractElement(vector: Operand, index: Operand): IRBuilder[Operand] =
    emitInstr(
      typeOf(vector) match {
        case VectorType(_, elementType) =>
          elementType;
        case _ => throw new Exception("Extract element expects vector type")
      },
      Instruction.ExtractElement(vector, index, Nil)
    )

  def insertElement(
      vector: Operand,
      element: Operand,
      index: Operand
  ): IRBuilder[Operand] =
    emitInstr(
      typeOf(vector),
      Instruction.InsertElement(vector, element, index, Nil)
    )

  def shuffleVector(
      vectorA: Operand,
      vectorB: Operand,
      mask: Constant
  ): IRBuilder[Operand] =
    // TODO: This return type is WRONG
    emitInstr(
      typeOf(vectorA),
      Instruction.ShuffleVector(vectorA, vectorB, mask, Nil)
    )

  def extractValue(aggregate: Operand, indices: List[Int]): IRBuilder[Operand] =
    emitInstr(
      extractValueType(indices, typeOf(aggregate)),
      Instruction.ExtractValue(aggregate, indices, Nil)
    )

  def insertValue(
      aggregate: Operand,
      element: Operand,
      indices: List[Int]
  ): IRBuilder[Operand] =
    emitInstr(
      typeOf(aggregate),
      Instruction.InsertValue(aggregate, element, indices, Nil)
    )

  def icmp(pred: IntegerPredicate, a: Operand, b: Operand): IRBuilder[Operand] =
    emitInstr(TypeAliases.i1, Instruction.ICmp(pred, a, b, Nil))

  def fcmp(
      pred: FloatingPointPredicate,
      a: Operand,
      b: Operand
  ): IRBuilder[Operand] =
    emitInstr(TypeAliases.i1, Instruction.FCmp(pred, a, b, Nil))

  def phi(incoming: List[(Operand, Name)]): IRBuilder[Operand] = {
    val ty = typeOf(incoming.head._1)
    emitInstr(ty, Instruction.Phi(ty, incoming, Nil))
  }

  def defaultCallInstruction(
      f: Operand,
      args: List[(Operand, List[ParameterAttribute])]
  ) = Instruction.Call(
    None, // tailCallKind
    CallingConvention.Fast,
    Nil, // returnAttributes
    Right(f), // callableOperand
    args,
    Nil, // functionAttributes
    Nil // metadata
  )

  def call(
      f: Operand,
      args: List[(Operand, List[ParameterAttribute])]
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
    emitInstr(typeOf(t), Instruction.Select(cond, t, f, Nil))

  def retVoid(): IRBuilder[Unit] =
    emitTerm(Terminator.Ret(None, Nil))

  def ret(ret: Operand): IRBuilder[Unit] =
    emitTerm(Terminator.Ret(Some(ret), Nil))

  def condBr(cond: Operand, trueDest: Name, falseDest: Name): IRBuilder[Unit] =
    emitTerm(Terminator.CondBr(cond, trueDest, falseDest, Nil))

  def br(target: Name): IRBuilder[Unit] =
    emitTerm(Terminator.Br(target, Nil))

  def switch(op: Operand, default: Name, destinations: List[(Constant, Name)]) =
    emitTerm(Terminator.Switch(op, default, destinations, Nil))

  def unreachable() =
    emitTerm(Terminator.Unreachable(Nil))
}
