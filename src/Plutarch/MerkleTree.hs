{-# OPTIONS_GHC -Wno-unused-imports #-}

module Plutarch.MerkleTree (validator, PHash (PHash), PMerkleTree (..),phash) where

import Plutarch.Api.V2
  ( PValidator,
  )
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (pmatchC)
import Plutarch.Prelude

-- import Plutarch.Bool (PPartialOrd)

newtype PHash (s :: S) = PHash (Term s PByteString)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq)

instance DerivePlutusType PHash where type DPTStrat _ = PlutusTypeNewtype

instance Semigroup (Term s PHash) where
  x' <> y' =
    phoistAcyclic
      ( plam $ \x y -> pmatch x $ \(PHash h0) ->
          pmatch y $ \(PHash h1) ->
            pcon $ PHash (h0 <> h1)
      )
      # x'
      # y'

instance Monoid (Term s PHash) where
  mempty = pcon $ PHash mempty

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
            PMerkleNode h0 _ _ -> pmatch r $ \case
              PMerkleNode h1 _ _ -> h0 #== h1
              _ -> pcon PFalse
            PMerkleLeaf h0 _ -> pmatch r $ \case
              PMerkleLeaf h1 _ -> h0 #== h1
              _ -> pcon PFalse
      )
      # l'
      # r'

phash :: forall (s :: S). Term s (PByteString :--> PHash)
phash = phoistAcyclic $ plam $ \bs ->
  pcon $ PHash (psha2_256 # bs)

validator :: ClosedTerm PValidator
validator = plam $ \_ _ _ -> popaque $ pconstant True
