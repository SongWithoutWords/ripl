module Index where

import Control.Monad.State
import Data.Map

import qualified Untyped.Ast as A0
import Indexed.Ast

data IndexState = IndexState
  { nextId :: Id
  , exps :: [Exp]
  } deriving(Eq, Ord, Show)

type Index = State IndexState

appendExp :: Exp -> Index Id
appendExp exp = do
  (IndexState nxt exps) <- get
  put $ IndexState (nxt + Id 1) (exp : exps)
  pure nxt

index :: A0.Ast -> Ast
index (A0.Ast exps) = undefined

indexNamedExps :: (Map String A0.Exp) -> Index (Map String Id)
indexNamedExps = mapM indexNamedExp

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

