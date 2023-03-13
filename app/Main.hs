module Main (main) where

import Data.Default (
  def,
 )
import Plutarch.MerkleTree qualified
import Ply.Plutarch (
  writeTypedScript,
 )

main :: IO ()
main = do
  writeTypedScript def "test" "./merkleeTree.plutus" Plutarch.MerkleTree.validator
