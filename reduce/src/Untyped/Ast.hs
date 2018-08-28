module Untyped.Ast where

import Data.Map

type NamedExps = Map String Exp

newtype Ast = Ast NamedExps
  deriving(Eq, Ord, Show)

data Exp
  = App Exp [Exp]
  | If Exp Exp Exp
  | TFun [Exp] Exp
  | Fun [(Exp, Exp)] (Maybe Exp) Exp
  | TStruct [(String, Exp)]
  | Struct [(String, Exp)]
  | Constructor Exp
  | Name String
  | Namespace NamedExps
  | VBln Bool
  | VInt Int
  | VStr String
  deriving(Eq, Ord, Show)
