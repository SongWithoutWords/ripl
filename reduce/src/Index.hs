{-# language TupleSections #-}

module Index where

import Control.Monad.State
import Data.Array
import qualified Data.Map as M

import qualified Untyped.Ast as A0
import Indexed.Ast

data IndexState = IndexState
  { nextId :: Id
  , exps :: [Exp]
  , scopes :: [M.Map String Id]
  } deriving(Eq, Ord, Show)

-- As an idea, what if for every expression I kept track of the index of it's parent?
-- This might be useful for: determining what variables are in scope,
-- determining the full names of things, and potentially other things. I think this might be really
-- great.

initialState = IndexState (Id 0) [] []

type Index = State IndexState

appendExp :: Exp -> Index Id
appendExp exp = do
  (IndexState next exps namespaces) <- get
  put $ IndexState (next + Id 1) (exp : exps) namespaces
  pure next

pushScope :: M.Map String Id -> Index ()
pushScope scope = do
  modify $ \s -> s { scopes = scope : scopes s}

index :: A0.Ast -> Ast
index exps =
  let (namedIds, (IndexState next exps' _)) = runState (indexNamedExps exps) initialState
  in Ast namedIds $ listArray (Id 0, next - Id 1) (reverse exps')

indexNamedExps :: A0.NamedExps -> Index (M.Map String Id)
indexNamedExps namedExps = M.fromList <$> mapM (\(n, exp) -> (n,) <$> indexNamedExp exp) namedExps

indexNamedExp :: A0.Exp -> Index Id
indexNamedExp exp = do
  exp' <- mapExp exp
  appendExp exp'

mapExp :: A0.Exp -> Index Exp
mapExp e = case e of

  A0.App f args -> liftM2 App (mapExp f) (mapM mapExp args)

  A0.If a b c -> liftM3 If (mapExp a) (mapExp b) (mapExp c)

  A0.TFun params ret -> liftM2 TFun (mapM mapExp params) (mapExp ret)

  A0.Fun params mret exp ->
    liftM3 Fun undefined (mapM mapExp mret) (mapExp exp)

  A0.Namespace exps -> Namespace <$> indexNamedExps exps

  A0.VInt i -> pure $ VInt i

