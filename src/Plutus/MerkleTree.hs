{-# LANGUAGE TemplateHaskell #-}

module Plutus.MerkleTree (
  fromList,
  toList,
  rootHash,
  isNull,
  size,
  mkProof,
  member,
  addLeaf,
  Hash (Hash),
  Proof,
  MerkleTree (MerkleEmpty, MerkleLeaf, MerkleNode),
)
where

import Control.Applicative (Alternative ((<|>)))
import Data.String (IsString)
import GHC.Generics (Generic)
import PlutusLedgerApi.V2 (LedgerBytes (LedgerBytes))
import PlutusTx qualified
import PlutusTx.Builtins (BuiltinByteString, appendByteString, divideInteger, sha2_256)
import PlutusTx.Foldable qualified
import PlutusTx.Prelude qualified
import Prettyprinter (Pretty)

-- Haskell types

type Proof = [Either Hash Hash]

newtype Hash = Hash BuiltinByteString
  deriving stock (Eq, Ord, Generic)
  deriving (IsString, Show, Pretty) via LedgerBytes

PlutusTx.makeIsDataIndexed ''Hash [('Hash, 0)]

data MerkleTree
  = MerkleEmpty
  | MerkleNode Hash MerkleTree MerkleTree
  | MerkleLeaf Hash BuiltinByteString
  deriving stock (Show)

instance Eq MerkleTree where
  MerkleEmpty == MerkleEmpty = True
  (MerkleLeaf h0 _) == (MerkleLeaf h1 _) = h0 == h1
  (MerkleNode h0 _ _) == (MerkleNode h1 _ _) = h0 == h1
  _ == _ = False

{- | Construct a 'MerkleTree' from a list of serialized data as
 'BuiltinByteString'.

 Note that, while this operation is doable on-chain, it is expensive and
 preferably done off-chain.
-}
fromList :: [BuiltinByteString] -> MerkleTree
fromList es0 = recursively (PlutusTx.Foldable.length es0) es0
  where
    recursively len =
      \case
        [] ->
          MerkleEmpty
        [e] ->
          MerkleLeaf (hash e) e
        es ->
          let cutoff = len `divideInteger` 2
              (l, r) = (PlutusTx.Prelude.take cutoff es, PlutusTx.Prelude.drop cutoff es)
              lnode = recursively cutoff l
              rnode = recursively (len - cutoff) r
           in MerkleNode (combineHash (rootHash lnode) (rootHash rnode)) lnode rnode

{- | Deconstruct a 'MerkleTree' back to a list of elements.

 >>> toList (fromList xs) == xs
 True
-}
toList :: MerkleTree -> [BuiltinByteString]
toList = go
  where
    go = \case
      MerkleEmpty -> []
      MerkleLeaf _ e -> [e]
      MerkleNode _ n1 n2 -> toList n1 <> toList n2

rootHash :: MerkleTree -> Hash
rootHash = \case
  MerkleEmpty -> hash ""
  MerkleLeaf h _ -> h
  MerkleNode h _ _ -> h

isNull :: MerkleTree -> Bool
isNull = \case
  MerkleEmpty -> True
  _ -> False

size :: MerkleTree -> Integer
size = \case
  MerkleEmpty -> 0
  MerkleNode _ l r -> size l + size r
  MerkleLeaf {} -> 1

{- | Construct a membership 'Proof' from an element and a 'MerkleTree'. Returns
 'Nothing' if the element isn't a member of the tree to begin with.
-}
mkProof :: BuiltinByteString -> MerkleTree -> Maybe Proof
mkProof e = go []
  where
    he = hash e
    go es = \case
      MerkleEmpty -> Nothing
      MerkleLeaf h _ ->
        if h == he
          then Just es
          else Nothing
      MerkleNode _ l r ->
        go (Right (rootHash r) : es) l <|> go (Left (rootHash l) : es) r
{-# INLINEABLE mkProof #-}

{- | Check whether a element is part of a 'MerkleTree' using only its root hash
 and a 'Proof'. The proof is guaranteed to be in log(n) of the size of the
 tree, which is why we are interested in such data-structure in the first
 place.
-}
member :: BuiltinByteString -> Hash -> Proof -> Bool
member e root = go (hash e)
  where
    go root' = \case
      [] -> root' == root
      (Left l) : q -> go (combineHash l root') q
      (Right r) : q -> go (combineHash root' r) q
{-# INLINEABLE member #-}

-- | Computes a SHA-256 hash of a given 'BuiltinByteString' message.
hash :: BuiltinByteString -> Hash
hash = Hash . sha2_256
{-# INLINEABLE hash #-}

{- | Combines two hashes digest into a new one. This is effectively a new hash
 digest of the same length.
-}
combineHash :: Hash -> Hash -> Hash
combineHash (Hash h) (Hash h') = hash (appendByteString h h')
{-# INLINEABLE combineHash #-}

addLeaf :: BuiltinByteString -> MerkleTree -> MerkleTree
addLeaf newData MerkleEmpty = MerkleLeaf (hash newData) newData
addLeaf newData (MerkleLeaf h dat) =
  let newDataHash = hash newData
      newLeaf = MerkleLeaf newDataHash newData
   in MerkleNode (combineHash h newDataHash) (MerkleLeaf h dat) newLeaf
addLeaf newData (MerkleNode h lnode rnode)
  | size lnode == size rnode = MerkleNode newHash lnode (addLeaf newData rnode)
  | size lnode > size rnode = MerkleNode newHash lnode (addLeaf newData rnode)
  | otherwise = MerkleNode newHash (addLeaf newData lnode) rnode
  where
    newHash = combineHash (hash newData) h
