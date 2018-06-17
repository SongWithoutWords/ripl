package ripl.llvm.irBuilder

import cats._
import cats.data.State
import cats.syntax.MonadOps
import cats.instances._

import ripl.llvm.ast._
import ripl.llvm.irBuilder.internal.SnocList

// I'm really not sure this justifies use of a state monad. Couldn't it be just a semigroup?
// Wouldn't it be possible to simply map over any collection of definitions?

// Oh well, just port it as-is first

object ModuleBuilderAliases {
  type ModuleBuilder[A] = State[ModuleBuilderState, A]
}
import ModuleBuilderAliases._

// newtype ModuleBuilderT m a = ModuleBuilderT { unModuleBuilderT :: StateT ModuleBuilderState m a }
//   deriving
//     ( Functor, Alternative, Applicative, Monad, MonadCont, MonadError e
//     , MonadFix, MonadIO, MonadPlus, MonadReader r, MonadTrans, MonadWriter w
//     )

// instance MonadFail m => MonadFail (ModuleBuilderT m) where
//   fail str = ModuleBuilderT (StateT $ \_ -> Fail.fail str)

case class ModuleBuilderState(definitions: SnocList[Definition])

// newtype ModuleBuilderState = ModuleBuilderState
//   { builderDefs :: SnocList Definition
//   }

// emptyModuleBuilder :: ModuleBuilderState
// emptyModuleBuilder = ModuleBuilderState
//   { builderDefs = mempty
//   }

// type ModuleBuilder = ModuleBuilderT Identity

// class Monad m => MonadModuleBuilder m where
//   liftModuleState :: State ModuleBuilderState a -> m a

//   default liftModuleState
//     :: (MonadTrans t, MonadModuleBuilder m1, m ~ t m1)
//     => State ModuleBuilderState a
//     -> m a
//   liftModuleState = lift . liftModuleState

// instance Monad m => MonadModuleBuilder (ModuleBuilderT m) where
//   liftModuleState (StateT s) = ModuleBuilderT $ StateT $ pure . runIdentity . s

// Evaluate 'ModuleBuilder' to a result and a list of definitions
// runModuleBuilder :: ModuleBuilderState -> ModuleBuilder a -> (a, [Definition])
// runModuleBuilder s m = runIdentity $ runModuleBuilderT s m

// Evaluate 'ModuleBuilderT' to a result and a list of definitions
// runModuleBuilderT :: Monad m => ModuleBuilderState -> ModuleBuilderT m a -> m (a, [Definition])
// runModuleBuilderT s (ModuleBuilderT m)
//   = second (getSnocList . builderDefs)
//   <$> runStateT m s

// Evaluate 'ModuleBuilder' to a list of definitions
// execModuleBuilder :: ModuleBuilderState -> ModuleBuilder a -> [Definition]
// execModuleBuilder s m = snd $ runModuleBuilder s m

// Evaluate 'ModuleBuilderT' to a list of definitions
// execModuleBuilderT :: Monad m => ModuleBuilderState -> ModuleBuilderT m a -> m [Definition]
// execModuleBuilderT s m = snd <$> runModuleBuilderT s m

case object ModuleBuilder {
  def emitDefn(defn: Definition): ModuleBuilder[Unit] =
    State.modify {
      case ModuleBuilderState(definitions) =>
        ModuleBuilderState(definitions.snoc(defn))
    }
// emitDefn :: MonadModuleBuilder m => Definition -> m ()
// emitDefn def = liftModuleState $ modify $ \s -> s { builderDefs = builderDefs s `snoc` def }

  type ParameterName = String
// A parameter name suggestion
// data ParameterName
//   = NoParameterName
//   | ParameterName ShortByteString
//   deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

// Using 'fromString` on non-ASCII strings will throw an error.
// instance IsString ParameterName where
//   fromString s
//     | all isAscii s = ParameterName (fromString s)
//     | otherwise =
//       error ("Only ASCII strings are automatically converted to LLVM parameter names. "
//       <> "Other strings need to be encoded to a `ShortByteString` using an arbitrary encoding.")

  def function(
      nm: Name,
      params: List[(Type, ParameterName)],
      retType: Type,
      bodyBuilder: (List[Operand] => IRBuilder[Unit])
  ): ModuleBuilder[Operand] = ???

// Define and emit a (non-variadic) function definition
// function
//   :: MonadModuleBuilder m
//   => Name  // ^ Function name
//   -> [(Type, ParameterName)]  // ^ Parameter types and name suggestions
//   -> Type  // ^ Return type
//   -> ([Operand] -> IRBuilderT m ())  // ^ Function body builder
//   -> m Operand
// function label argtys retty body = do
//   let tys = fst <$> argtys
//   (paramNames, blocks) <- runIRBuilderT emptyIRBuilder $ do
//     paramNames <- forM argtys $ \(_, paramName) -> case paramName of
//       NoParameterName -> fresh
//       ParameterName p -> fresh `named` p
//     body $ zipWith LocalReference tys paramNames
//     return paramNames
//   let
//     def = GlobalDefinition functionDefaults
//       { name        = label
//       , parameters  = (zipWith (\ty nm -> Parameter ty nm []) tys paramNames, False)
//       , returnType  = retty
//       , basicBlocks = blocks
//       }
//     funty = ptr $ FunctionType retty (fst <$> argtys) False
//   emitDefn def
//   pure $ ConstantOperand $ C.GlobalReference funty label

// An external function definition
// extern
//   :: MonadModuleBuilder m
//   => Name   // ^ Definition name
//   -> [Type] // ^ Parametere types
//   -> Type   // ^ Type
//   -> m Operand
// extern nm argtys retty = do
//   emitDefn $ GlobalDefinition functionDefaults
//     { name        = nm
//     , linkage     = External
//     , parameters  = ([Parameter ty (mkName "") [] | ty <- argtys], False)
//     , returnType  = retty
//     }
//   let funty = ptr $ FunctionType retty argtys False
//   pure $ ConstantOperand $ C.GlobalReference funty nm

// A named type definition
// typedef
//   :: MonadModuleBuilder m
//   => Name
//   -> Maybe Type
//   -> m ()
// typedef nm ty = do
//   emitDefn $ TypeDefinition nm ty
//   pure ()

}

// Convenience function for module construction
// buildModule :: ShortByteString -> ModuleBuilder a -> Module
// buildModule nm = mkModule . execModuleBuilder emptyModuleBuilder
//   where
//     mkModule ds = defaultModule { moduleName = nm, moduleDefinitions = ds }

// Convenience function for module construction (transformer version)
// buildModuleT :: Monad m => ShortByteString -> ModuleBuilderT m a -> m Module
// buildModuleT nm = fmap mkModule . execModuleBuilderT emptyModuleBuilder
//   where
//     mkModule ds = defaultModule { moduleName = nm, moduleDefinitions = ds }

//-----------------------------------------------------------------------------
// mtl instances
//-----------------------------------------------------------------------------

// instance MonadState s m => MonadState s (ModuleBuilderT m) where
//   state = lift . state

// instance MonadModuleBuilder m => MonadModuleBuilder (ContT r m)
// instance MonadModuleBuilder m => MonadModuleBuilder (ExceptT e m)
// instance MonadModuleBuilder m => MonadModuleBuilder (IdentityT m)
// instance MonadModuleBuilder m => MonadModuleBuilder (ListT m)
// instance MonadModuleBuilder m => MonadModuleBuilder (MaybeT m)
// instance MonadModuleBuilder m => MonadModuleBuilder (ReaderT r m)
// instance (MonadModuleBuilder m, Monoid w) => MonadModuleBuilder (Strict.RWST r w s m)
// instance (MonadModuleBuilder m, Monoid w) => MonadModuleBuilder (Lazy.RWST r w s m)
// instance MonadModuleBuilder m => MonadModuleBuilder (StateT s m)
// instance MonadModuleBuilder m => MonadModuleBuilder (Lazy.StateT s m)
// instance (Monoid w, MonadModuleBuilder m) => MonadModuleBuilder (Strict.WriterT w m)
// instance (Monoid w, MonadModuleBuilder m) => MonadModuleBuilder (Lazy.WriterT w m)
