module Main (main) where

import Spec.MerkleTreeSpec (unitTest)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  defaultMain $
    testGroup
      "Unit Test Group"
      [ unitTest
      ]
