module Main (main) where

import Spec.MerkleTreeSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  defaultMain $
    testGroup
      "Unit Test Group"
      [ Spec.MerkleTreeSpec.unitTest
      , Spec.MerkleTreeSpec.merkleTreeProperties
      ]
