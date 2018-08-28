{-# language GeneralizedNewtypeDeriving #-}

module Indexed.Ast where

import Data.Map

newtype Id = Id Word
  deriving(Eq, Num, Ord, Show)

data Exp
  = App Exp [Exp]
  | If Exp Exp Exp
  | TFun [Exp] Exp
  | Fun [(Exp, Exp)] (Maybe Exp) Exp
  | TStruct [(String, Exp)]
  | Struct [(String, Exp)]
  | Constructor Exp
  | Name String [Id]
  | Namespace (Map String Id)
  | VBln Bool
  | VInt Int
  | VStr String
  deriving(Eq, Ord, Show)
