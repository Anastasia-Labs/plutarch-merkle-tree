module Main (main) where

import Plutarch.MerkleTree qualified
import Data.Default (
  def,
 )
import Ply.Plutarch (
  writeTypedScript,
 )

main :: IO ()
main = do
  writeTypedScript def "test" "./merkleeTree.plutus" Plutarch.MerkleTree.validator
