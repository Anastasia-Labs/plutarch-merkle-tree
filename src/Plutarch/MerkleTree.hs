{-# OPTIONS_GHC -Wno-unused-imports #-}

module Plutarch.MerkleTree (validator, PHash (PHash), PMerkleTree (..)) where

import Plutarch.Api.V2 (
  PValidator,
 )
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (pmatchC)
import Plutarch.Prelude

-- import Plutarch.Bool (PPartialOrd)

newtype PHash (s :: S) = PHash (Term s PByteString)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq)

instance DerivePlutusType PHash where type DPTStrat _ = PlutusTypeNewtype

-- instance PEq (PHash) where
-- (#==) ( PHash x ) ( PHash y ) = x #== y

data PMerkleTree (s :: S)
  = PMerkleEmpty
  | PMerkleNode (Term s PHash) (Term s PMerkleTree) (Term s PMerkleTree)
  | PMerkleLeaf (Term s PHash) (Term s PByteString)
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType PMerkleTree where type DPTStrat _ = PlutusTypeScott

instance PEq PMerkleTree where
  l' #== r' =
    phoistAcyclic
      ( plam $ \l r ->
          pmatch l $ \case
            PMerkleEmpty -> pmatch r $ \case
              PMerkleEmpty -> pcon PTrue
              _ -> pcon PFalse
            PMerkleNode {} -> pcon PFalse
            PMerkleLeaf _ _ -> pcon PFalse
      )
      # l'
      # r'

-- PMerkleEmpty #== PMerkleEmpty = pconstant True
-- (PMerkleLeaf h0 _) #== (PMerkleLeaf h1 _) = _a
-- (PMerkleNode h0 _ _) #== (PMerkleNode h1 _ _) = h0 #== h1
-- _ #== _ = pcon PFalse

validator :: ClosedTerm PValidator
validator = plam $ \_ _ _ -> popaque $ pconstant True
