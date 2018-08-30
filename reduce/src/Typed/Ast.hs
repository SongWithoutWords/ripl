module Typed.Ast where

import Data.Array
import Data.Map

import Indexed.Ast(Id)

data Ast = Ast (Array Id Exp)
  deriving(Eq, Ord, Show)

data Exp
  = App Exp [Exp]
  | If Exp Exp Exp
  | TFun [Exp] Exp
  | Fun [(Exp, Exp)] (Maybe Exp) Exp
  | TStruct [(String, Exp)]
  | Struct [(String, Exp)]
  | Constructor Exp
  | Name String [Id]
  | Namespace -- (Map String Id)
  | VBln Bool
  | VInt Int
  | VStr String
  deriving(Eq, Ord, Show)
