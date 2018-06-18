// {-# LANGUAGE DefaultSignatures #-}
// {-# LANGUAGE FlexibleContexts #-}
// {-# LANGUAGE FlexibleInstances #-}
// {-# LANGUAGE GADTs #-}
// {-# LANGUAGE GeneralizedNewtypeDeriving #-}
// {-# LANGUAGE MultiParamTypeClasses #-}
// {-# LANGUAGE UndecidableInstances #-} // For MonadState s (ModuleBuilderT m) instance

// module LLVM.IRBuilder.Monad where
package ripl.llvm.irBuilder

import cats._
import cats.data.State
import cats.syntax.MonadOps
// import cats.MonadState
import cats.instances._

import ripl.llvm.ast._
import ripl.llvm.irBuilder.Internal.SnocList

// import LLVM.Prelude

// import Control.Monad.Cont
// import Control.Monad.Except
// import Control.Monad.Fail
// import qualified Control.Monad.Fail as Fail
// import Control.Monad.Identity
// import Control.Monad.Writer.Lazy as Lazy
// import Control.Monad.Writer.Strict as Strict
// import Control.Monad.Reader
// import Control.Monad.RWS.Lazy as Lazy
// import Control.Monad.RWS.Strict as Strict
// import qualified Control.Monad.State.Lazy as Lazy
// import Control.Monad.State.Strict
// import Control.Monad.List
// import Control.Monad.Trans.Maybe
// import Control.Monad.Trans.Identity

// import Data.Bifunctor
// import Data.String
// import Data.HashSet(HashSet)
// import qualified Data.HashSet as HS

// import LLVM.AST

// import LLVM.IRBuilder.Internal.SnocList

// This provides a uniform API for creating instructions and inserting them
// into a basic block: either at the end of a BasicBlock, or at a specific
// location in a block.
// newtype IRBuilderT m a = IRBuilderT { unIRBuilderT :: StateT IRBuilderState m a }
//   deriving
//     ( Functor, Alternative, Applicative, Monad, MonadCont, MonadError e
//     , MonadFix, MonadIO, MonadPlus, MonadReader r, MonadTrans, MonadWriter w
//     )

// instance MonadFail m => MonadFail (IRBuilderT m) where
// fail str = IRBuilderT (StateT $ \ _ -> Fail.fail str)

object IRBuilderAliases {
  type IRBuilder[a] = State[IRBuilderState, a]
}
import IRBuilderAliases._

// type IRBuilder = IRBuilderT Identity

// class Monad m => MonadIRBuilder m where
//   liftIRState :: State IRBuilderState a -> m a

//   default liftIRState
//     :: (MonadTrans t, MonadIRBuilder m1, m ~ t m1)
//     => State IRBuilderState a
//     -> m a
//   liftIRState = lift . liftIRState

// instance Monad m => MonadIRBuilder (IRBuilderT m) where
// liftIRState (StateT s) = IRBuilderT $ StateT $ pure . runIdentity . s

// A partially constructed block as a sequence of instructions
case class PartialBlock(
    partialBlockName: Name,
    partialBlockInstrs: SnocList[Named[Instruction]],
    partialBlockTerm: Option[Named[Terminator]]
)

case object PartialBlock {
  def empty(name: Name) = PartialBlock(name, SnocList(Nil), None)
}
// emptyPartialBlock :: Name -> PartialBlock
// emptyPartialBlock nm = PartialBlock nm mempty Nothing

// Builder monad state
case class IRBuilderState(
    builderSupply: Int,
    // builderUsedNames: Set[String],
    builderUsedNames: Map[String, Int],
    builderNameSuggestion: Option[String],
    builderBlocks: SnocList[BasicBlock],
    builderBlock: Option[PartialBlock]
)

case object IRBuilderState {
  val empty = IRBuilderState
  (0, Set(), None, SnocList(Nil), None)
}

case object runIRBuilder {
  def apply[A](
      initialState: IRBuilderState,
      computation: IRBuilder[A]
  ): (A, List[BasicBlock]) = {
    val (finalState, result) = computation.run(initialState).value
    (result, finalState.builderBlocks.getSnocList)
  }
}

// Evaluate IRBuilder to a result and a list of basic blocks
// runIRBuilder :: IRBuilderState -> IRBuilder a -> (a, [BasicBlock])
// runIRBuilder s m = runIdentity $ runIRBuilderT s m

// Evaluate IRBuilderT to a result and a list of basic blocks
// runIRBuilderT :: Monad m => IRBuilderState -> IRBuilderT m a -> m (a, [BasicBlock])
// runIRBuilderT s m
//   = second (getSnocList . builderBlocks)
//   <$> runStateT (unIRBuilderT $ m <* block) s

case object execIRBuilder {
  def apply[A](
      initialState: IRBuilderState,
      computation: IRBuilder[A]
  ): List[BasicBlock] =
    runIRBuilder(initialState, computation)._2
}

// Evaluate IRBuilder to a list of basic blocks
// execIRBuilder :: IRBuilderState -> IRBuilder a -> [BasicBlock]
// execIRBuilder s m = snd $ runIRBuilder s m

// Evaluate IRBuilderT to a list of basic blocks
// execIRBuilderT :: Monad m => IRBuilderState -> IRBuilderT m a -> m [BasicBlock]
// execIRBuilderT s m = snd <$> runIRBuilderT s m

//////////////////////////////////////////////////////////////////////////////-
// * Low-level functionality
//////////////////////////////////////////////////////////////////////////////-

case object modifyBlock {
  def apply(f: (PartialBlock) => PartialBlock): IRBuilder[Unit] =
    for {
      mbb <- State.inspect((s: IRBuilderState) => s.builderBlock)
      _ <- mbb match {
        case None =>
          for {
            nm <- freshUnName()
            _ <- State.modify { (s: IRBuilderState) =>
              s.copy(builderBlock = Some(f(PartialBlock.empty(nm))))
            }
          } yield ()
        case Some(bb) =>
          for {
            _ <- State.modify { (s: IRBuilderState) =>
              s.copy(builderBlock = Some(f(bb)))
            }
          } yield ()
      }
    } yield ()
}

// modifyBlock
//   :: MonadIRBuilder m
//   => (PartialBlock -> PartialBlock)
//   -> m ()
// modifyBlock f = do
//   mbb <- liftIRState $ gets builderBlock
//   case mbb of
//     Nothing -> do
//       nm <- freshUnName
//       liftIRState $ modify $ \s -> s { builderBlock = Just $! f $ emptyPartialBlock nm }
//     Just bb ->
//       liftIRState $ modify $ \s -> s { builderBlock = Just $! f bb }

// Generate a fresh name. The resulting name is numbered or
// based on the name suggested with 'named' if that's used.
// fresh :: MonadIRBuilder m => m Name
// fresh = do
//   msuggestion <- liftIRState $ gets builderNameSuggestion
//   maybe freshUnName freshName msuggestion

case object fresh {
  def apply(): IRBuilder[Name] =
    for {
      msuggestion <- State.inspect(
        (s: IRBuilderState) => s.builderNameSuggestion
      )
      name <- msuggestion match {
        case Some(suggestion) => freshName(suggestion)
        case None             => freshUnName()
      }
    } yield (name)
}

case object freshUnName {
  def apply(): IRBuilder[Name] =
    for {
      n <- State.inspect { s: IRBuilderState =>
        s.builderSupply
      }
      _ <- State.modify { s: IRBuilderState =>
        s.copy(builderSupply = s.builderSupply + 1)
      }
    } yield (Name(n.toString()))
}

case object freshName {
  def apply(hint: String): IRBuilder[Name] =
    for {
      usedNames <- State.inspect { s: IRBuilderState =>
        s.builderUsedNames
      }
      nameUsedCount = usedNames.get(hint) match {
        case None    => 0
        case Some(n) => n + 1
      }
      _ <- State.modify { (s: IRBuilderState) =>
        s.copy(builderUsedNames = usedNames + Tuple2(hint, nameUsedCount))
      }
    } yield
      (Name(hint + (if (nameUsedCount > 0) nameUsedCount.toString else "")))

}

// Generate a fresh name from a name suggestion
// freshName :: MonadIRBuilder m => ShortByteString -> m Name
// freshName suggestion = do
//   usedNames <- liftIRState $ gets builderUsedNames
//   let
//     candidates = suggestion : [suggestion <> fromString (show n) | n <- [(1 :: Int)..]]
//     (unusedName:_) = filter (not . (`HS.member` usedNames)) candidates
//   liftIRState $ modify $ \s -> s { builderUsedNames = HS.insert unusedName $ builderUsedNames s }
//   return $ Name unusedName

// Generate a fresh numbered name
// freshUnName :: MonadIRBuilder m => m Name
// freshUnName = liftIRState $ do
//   n <- gets builderSupply
//   modify $ \s -> s { builderSupply = 1 + n }
//   pure $ UnName n

case object emitInstr {
  def apply(returnType: Type, instruction: Instruction): IRBuilder[Operand] =
    for {
      nm <- fresh()
      _ <- modifyBlock { p: PartialBlock =>
        p.copy(
          partialBlockInstrs = p.partialBlockInstrs.snoc(:=(nm, instruction))
        )
      }
    } yield (LocalReference(returnType, nm))
}

// Emit instruction
// emitInstr
//   :: MonadIRBuilder m
//   => Type // ^ Return type
//   -> Instruction
//   -> m Operand
// emitInstr retty instr = do
//   nm <- fresh
//   modifyBlock $ \bb -> bb
//     { partialBlockInstrs = partialBlockInstrs bb `snoc` (nm := instr)
//     }
//   pure (LocalReference retty nm)

case object emitInstrVoid {
  def apply(instruction: Instruction): IRBuilder[Unit] =
    modifyBlock { p: PartialBlock =>
      p.copy(partialBlockInstrs = p.partialBlockInstrs.snoc(Do(instruction)))
    }
}

// Emit instruction that returns void
// emitInstrVoid
//   :: MonadIRBuilder m
//   => Instruction
//   -> m ()
// emitInstrVoid instr = do
//   modifyBlock $ \bb -> bb
//     { partialBlockInstrs = partialBlockInstrs bb `snoc` (Do instr)
//     }
//   pure ()

case object emitTerm {
  def apply(t: Terminator): IRBuilder[Unit] =
    modifyBlock { p: PartialBlock =>
      p.copy(partialBlockTerm = Some(Do(t)))
    }
}

// Emit terminator
// emitTerm
//   :: MonadIRBuilder m
//   => Terminator
//   -> m ()
// emitTerm term = modifyBlock $ \bb -> bb
//   { partialBlockTerm = Just (Do term)
//   }

case object emitBlockStart {
  def apply(nm: Name): IRBuilder[Unit] =
    for {
      mbb <- State.inspect { s: IRBuilderState =>
        s.builderBlock
      }
      _ <- mbb match {
        case Some(partialBlock) =>
          val instrs = partialBlock.partialBlockInstrs
          val terminator: Named[Terminator] =
            partialBlock.partialBlockTerm match {
              case None       => Do(Terminator.Ret(None, InstructionMetadata()))
              case Some(term) => term
            }
          for {
            _ <- State.modify { s: IRBuilderState =>
              s.copy(
                builderBlocks = s.builderBlocks.snoc(
                  BasicBlock(
                    partialBlock.partialBlockName,
                    instrs.getSnocList,
                    terminator
                  )
                )
              )
            }
          } yield ()
        case None => //State.modify(s: IRBuilderState => s)
          Monad[IRBuilder].pure(())
      }
      _ <- State.modify { s: IRBuilderState =>
        s.copy(builderBlock = Some(PartialBlock.empty(nm)))
      }
    } yield ()
}

// Starts a new block labelled using the given name and ends the previous
// one. The name is assumed to be fresh.
// emitBlockStart
//   :: MonadIRBuilder m
//   => Name
//   -> m ()
// emitBlockStart nm = do
//   mbb <- liftIRState $ gets builderBlock
//   case mbb of
//     Nothing -> return ()
//     Just bb -> do
//       let
//         instrs = getSnocList $ partialBlockInstrs bb
//         newBb = case partialBlockTerm bb of
//           Nothing   -> BasicBlock (partialBlockName bb) instrs (Do (Ret Nothing []))
//           Just term -> BasicBlock (partialBlockName bb) instrs term
//       liftIRState $ modify $ \s -> s
//         { builderBlocks = builderBlocks s `snoc` newBb
//         }
//   liftIRState $ modify $ \s -> s { builderBlock = Just $ emptyPartialBlock nm }

//////////////////////////////////////////////////////////////////////////////-
// * High-level functionality
//////////////////////////////////////////////////////////////////////////////-

// Starts a new block and ends the previous one
case object block {
  def apply(): IRBuilder[Name] =
    for {
      nm <- fresh()
      _  <- emitBlockStart(nm)
    } yield (nm)
}
// block
//   :: MonadIRBuilder m
//   => m Name
// block = do
//   nm <- fresh
//   emitBlockStart nm
//   return nm

// @ir `named` name@ executes the 'IRBuilder' @ir@ using @name@ as the base
// name whenever a fresh local name is generated. Collisions are avoided by
// appending numbers (first @"name"@, then @"name1"@, @"name2"@, and so on).

case object named {
  def apply[A](ir: IRBuilder[A], name: String): IRBuilder[A] =
    for {
      before <- State.inspect { s: IRBuilderState =>
        s.builderNameSuggestion
      }
      _ <- State.modify { s: IRBuilderState =>
        s.copy(builderNameSuggestion = Some(name))
      }
      result <- ir
      _ <- State.modify { s: IRBuilderState =>
        s.copy(builderNameSuggestion = before)
      }
    } yield (result)
}

// named
//   :: MonadIRBuilder m
//   => m r
//   -> ShortByteString
//   -> m r
// named ir name = do
//   before <- liftIRState $ gets builderNameSuggestion
//   liftIRState $ modify $ \s -> s { builderNameSuggestion = Just name }
//   result <- ir
//   liftIRState $ modify $ \s -> s { builderNameSuggestion = before }
//   return result

//////////////////////////////////////////////////////////////////////////////-
// mtl instances
//////////////////////////////////////////////////////////////////////////////-

// instance MonadState s m => MonadState s (IRBuilderT m) where
//   state = lift . state

// instance MonadIRBuilder m => MonadIRBuilder (ContT r m)
// instance MonadIRBuilder m => MonadIRBuilder (ExceptT e m)
// instance MonadIRBuilder m => MonadIRBuilder (IdentityT m)
// instance MonadIRBuilder m => MonadIRBuilder (ListT m)
// instance MonadIRBuilder m => MonadIRBuilder (MaybeT m)
// instance MonadIRBuilder m => MonadIRBuilder (ReaderT r m)
// instance (MonadIRBuilder m, Monoid w) => MonadIRBuilder (Strict.RWST r w s m)
// instance (MonadIRBuilder m, Monoid w) => MonadIRBuilder (Lazy.RWST r w s m)
// instance MonadIRBuilder m => MonadIRBuilder (StateT s m)
// instance MonadIRBuilder m => MonadIRBuilder (Lazy.StateT s m)
// instance (Monoid w, MonadIRBuilder m) => MonadIRBuilder (Strict.WriterT w m)
// instance (Monoid w, MonadIRBuilder m) => MonadIRBuilder (Lazy.WriterT w m)
