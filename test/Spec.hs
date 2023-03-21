module Main (main) where

import Spec.MerkleTreeSpec (scriptEvaluation)

main :: IO ()
main = do
  case scriptEvaluation of
    Left a -> print a
    Right b -> print b
