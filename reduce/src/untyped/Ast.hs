module Untyped.Ast where

import Data.Map

data Exp
  = App Exp [Exp]
  | If Exp Exp Exp
  | TFun [Exp] Exp
  | Fun [(Exp, Exp)] (Maybe Exp) Exp
  | Name String
  | Namespace (Map String Exp)
  | VBln Bool
  | VInt Int
  | VStr String
  deriving(Eq, Ord, Show)
