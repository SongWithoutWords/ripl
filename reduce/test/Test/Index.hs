module Test.Index(tests) where

import qualified Data.Array as A
import qualified Data.Map as M

import Test.Tasty
import Test.Tasty.HUnit

import qualified Untyped.Ast as A0
import Index
import Indexed.Ast

test :: String -> [(String, A0.Exp)] -> ([(String, Id)], [Exp]) -> TestTree
test name input (namedIds, exps) = testCase name $
  (index input) @?=
  (Ast (M.fromList namedIds) (A.listArray (Id 0, Id $ fromIntegral $ length exps - 1) exps))

tests :: TestTree
tests = testGroup "Index"
  [ test "simple"
    [ ("a", A0.VInt 1)
    , ("b", A0.VInt 2)
    ]
    (
      [ ("a", Id 0)
      , ("b", Id 1)
      ],
      [ VInt 1
      , VInt 2
      ]
    )
  , test "with namespaces"
    [("n", A0.Namespace
        [ ("a", A0.VInt 1)
        , ("b", A0.VInt 2)
        ]
     )
    , ("c", A0.VInt 3)
    ]
    (
      [ ("n", Id 2)
      , ("c", Id 3)
      ],
      [ VInt 1
      , VInt 2
      , Namespace $ M.fromList [("a", Id 0), ("b", Id 1)]
      , VInt 3
      ]
    )
  ]
