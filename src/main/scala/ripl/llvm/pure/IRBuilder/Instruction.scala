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
    val constantOperands: List[Constant] = offsets.collect {
      case ConstantOperand(c) => c;
      case _                  => throw new Exception()
    }
    val gepType = getElementType(typeOf(addr))
    emitInstr(gepType, Instruction.GetElementPtr(false, addr, offsets, Nil))

    // TODO: Make same change in llvm-hs code-base
  }

  def trunc(a: Operand, to: Type): IRBuilder[Operand] =
    emitInstr(to, Instruction.Trunc(a, to, Nil))

  def fpTrunc(a: Operand, to: Type): IRBuilder[Operand] =
    emitInstr(to, Instruction.FPTrunc(a, to, Nil))

// zext :: MonadIRBuilder m => Operand -> Type -> m Operand
// zext a to = emitInstr to $ ZExt a to []

// sext :: MonadIRBuilder m => Operand -> Type -> m Operand
// sext a to = emitInstr to $ SExt a to []

// fptoui :: MonadIRBuilder m => Operand -> Type -> m Operand
// fptoui a to = emitInstr to $ FPToUI a to []

// fptosi :: MonadIRBuilder m => Operand -> Type -> m Operand
// fptosi a to = emitInstr to $ FPToSI a to []

// fpext :: MonadIRBuilder m => Operand -> Type -> m Operand
// fpext a to = emitInstr to $ FPExt a to []

// uitofp :: MonadIRBuilder m => Operand -> Type -> m Operand
// uitofp a to = emitInstr to $ UIToFP a to []

// sitofp :: MonadIRBuilder m => Operand -> Type -> m Operand
// sitofp a to = emitInstr to $ SIToFP a to []

// ptrtoint :: MonadIRBuilder m => Operand -> Type -> m Operand
// ptrtoint a to = emitInstr to $ PtrToInt a to []

// inttoptr :: MonadIRBuilder m => Operand -> Type -> m Operand
// inttoptr a to = emitInstr to $ IntToPtr a to []

// bitcast :: MonadIRBuilder m => Operand -> Type -> m Operand
// bitcast a to = emitInstr to $ BitCast a to []

// extractElement :: MonadIRBuilder m => Operand -> Operand -> m Operand
// extractElement v i = emitInstr (typeOf v) $ ExtractElement v i []

// insertElement :: MonadIRBuilder m => Operand -> Operand -> Operand -> m Operand
// insertElement v e i = emitInstr (typeOf v) $ InsertElement v e i []

// shuffleVector :: MonadIRBuilder m => Operand -> Operand -> C.Constant -> m Operand
// shuffleVector a b m = emitInstr (typeOf a) $ ShuffleVector a b m []

// extractValue :: MonadIRBuilder m => Operand -> [Word32] -> m Operand
// extractValue a i = emitInstr (typeOf a) $ ExtractValue a i []

// insertValue :: MonadIRBuilder m => Operand -> Operand -> [Word32] -> m Operand
// insertValue a e i = emitInstr (typeOf a) $ InsertValue a e i []

// icmp :: MonadIRBuilder m => IP.IntegerPredicate -> Operand -> Operand -> m Operand
// icmp pred a b = emitInstr i1 $ ICmp pred a b []

// fcmp :: MonadIRBuilder m => FP.FloatingPointPredicate -> Operand -> Operand -> m Operand
// fcmp pred a b = emitInstr i1 $ FCmp pred a b []

// -- | Unconditional branch
// br :: MonadIRBuilder m => Name -> m ()
// br val = emitTerm (Br val [])

// phi :: MonadIRBuilder m => [(Operand, Name)] -> m Operand
// phi [] = emitInstr AST.void $ Phi AST.void [] []
// phi incoming@(i:_) = emitInstr ty $ Phi ty incoming []
//   where
//     ty = typeOf (fst i) -- result type

// retVoid :: MonadIRBuilder m => m ()
// retVoid = emitTerm (Ret Nothing [])

// call :: MonadIRBuilder m => Operand -> [(Operand, [ParameterAttribute])] -> m Operand
// call fun args = do
//   let instr = Call {
//     AST.tailCallKind = Nothing
//   , AST.callingConvention = CC.C
//   , AST.returnAttributes = []
//   , AST.function = Right fun
//   , AST.arguments = args
//   , AST.functionAttributes = []
//   , AST.metadata = []
//   }
//   case typeOf fun of
//       FunctionType r _ _ -> case r of
//         VoidType -> emitInstrVoid instr >> (pure (ConstantOperand (C.Undef void)))
//         _        -> emitInstr r instr
//       PointerType (FunctionType r _ _) _ -> case r of
//         VoidType -> emitInstrVoid instr >> (pure (ConstantOperand (C.Undef void)))
//         _        -> emitInstr r instr
//       _ -> error "Cannot call non-function (Malformed AST)."

// ret :: MonadIRBuilder m => Operand -> m ()
// ret val = emitTerm (Ret (Just val) [])

// switch :: MonadIRBuilder m => Operand -> Name -> [(C.Constant, Name)] -> m ()
// switch val def dests = emitTerm $ Switch val def dests []

// select :: MonadIRBuilder m => Operand -> Operand -> Operand -> m Operand
// select cond t f = emitInstr (typeOf t) $ Select cond t f []

// condBr :: MonadIRBuilder m => Operand -> Name -> Name -> m ()
// condBr cond tdest fdest = emitTerm $ CondBr cond tdest fdest []

// unreachable :: MonadIRBuilder m => m ()
// unreachable = emitTerm $ Unreachable []
}
