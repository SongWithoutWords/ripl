import Test.Tasty

import qualified Test.Index

main :: IO ()
main = defaultMain $ testGroup "Tests" [Test.Index.tests]
