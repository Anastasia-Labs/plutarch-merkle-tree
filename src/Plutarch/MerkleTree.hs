module Plutarch.MerkleTree (validator, PHash (PHash), PMerkleTree (..), phash, pmember, mkProof, proof, isMember) where

import Plutarch.Api.V2 (
  PValidator,
 )
import Plutarch.Extra.Applicative ((#<!>))
import "liqwid-plutarch-extra" Plutarch.Extra.List (pisSingleton)
import Plutarch.Maybe (pfromJust)
import Plutarch.Prelude

-- import Plutarch.Bool (PPartialOrd)
-- import "plutarch-extra" Plutarch.Extra.List (preverse)
-- import Numeric.Natural (Natural)

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

_pfromList :: forall (s :: S). Term s (PList PByteString :--> PMerkleTree)
_pfromList = phoistAcyclic $
  plam $ \x ->
    let go = pfix #$ plam $ \_self len list ->
          pmatch list $ \case
            PSNil -> pcon PMerkleEmpty
            ls ->
              pif
                (pisSingleton #$ pcon ls)
                -- (pmatch (pcon ls ) $ \(PSCons v _) -> pcon $ PMerkleLeaf (phash # v) v)
                (plet (phead #$ pcon ls) $ \v -> pcon $ PMerkleLeaf (phash # v) v)
                ( plet (pdiv # len # 2) $ \_cutoff ->
                    -- let left = pdrop (cutoff ) $ preverse # pcon ls
                    -- in
                    pcon PMerkleEmpty
                    -- let left = preverse $ pdrop cutoff $ preverse ls
                    --     right = pdrop cutoff ls
                    --     leftnode = self # cutoff # left
                    --     rightnode = self # cutoff # right
                    --  in PMerkleNode (prootHash # leftnode) leftnode rightnode
                )
     in -- (undefined)
        go # (plength # x) # x

type PProof = PList (PEither PHash PHash)

prootHash :: forall (s :: S). Term s (PMerkleTree :--> PHash)
prootHash = phoistAcyclic $ plam $ \m ->
  pmatch m $ \case
    PMerkleEmpty -> mempty
    PMerkleLeaf h _ -> h
    PMerkleNode h _ _ -> h

_peitherOf :: forall (s :: S) (a :: PType). Term s (PMaybe a :--> PMaybe a :--> PMaybe a)
_peitherOf = phoistAcyclic $
  plam $ \l r ->
    pmatch l $ \case
      PJust x -> pcon $ PJust x
      PNothing -> pmatch r $ \case
        PJust y -> pcon $ PJust y
        PNothing -> pcon PNothing

mkProof :: forall (s :: S). Term s (PByteString :--> PMerkleTree :--> PMaybe PProof)
mkProof = phoistAcyclic $
  plam $ \bs -> plet (phash # bs) $ \he ->
    let go = pfix #$ plam $ \self es merkT ->
          pmatch merkT $ \case
            PMerkleEmpty -> pcon PNothing
            PMerkleLeaf h _ ->
              pif
                (h #== he)
                (pcon $ PJust es)
                (pcon PNothing)
            PMerkleNode _ l r -> (self # pcon (PSCons (pcon $ PRight (prootHash # r)) es) # l) #<!> (self # pcon (PSCons (pcon $ PLeft (prootHash # l)) es) # r)
     in go # pcon PSNil

pmember :: forall (s :: S). Term s (PByteString :--> PHash :--> PProof :--> PBool)
pmember = phoistAcyclic $
  plam $ \bs root ->
    let go = pfix #$ plam $ \self root' proof ->
          pmatch proof $ \case
            PSNil -> root' #== root
            PSCons x xs -> pmatch x $ \case
              PLeft l -> self # (l <> root') # xs
              PRight r -> self # (root' <> r) # xs
     in go # (phash # bs)

phash :: forall (s :: S). Term s (PByteString :--> PHash)
phash = phoistAcyclic $ plam $ \bs ->
  pcon $ PHash (psha2_256 # bs)

validator :: ClosedTerm PValidator
validator = plam $ \_ _ _ -> popaque $ pconstant True

mt :: forall {s :: S}. Term s PMerkleTree
mt = pcon $ PMerkleLeaf (phash #$ phexByteStr "41") (phexByteStr "41")

proof :: forall {s :: S}. Term s PProof
proof = pfromJust #$ mkProof # phexByteStr "41" # mt

rh :: forall {s :: S}. Term s PHash
rh = prootHash # mt

isMember :: forall {s :: S}. Term s PBool
isMember = pmember # phexByteStr "41" # rh # proof
