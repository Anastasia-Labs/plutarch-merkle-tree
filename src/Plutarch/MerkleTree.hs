{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.MerkleTree (
  PHash (PHash),
  PMerkleTree (PMerkleEmpty, PMerkleLeaf, PMerkleNode),
  PProof,
  PEitherData (PDLeft, PDRight),
  phash,
  pmember,
  _pdrop,
)
where

import Plutarch.DataRepr (DerivePConstantViaData (DerivePConstantViaData), PDataFields)
import Plutarch.EitherData (PEitherData (PDLeft, PDRight))
import Plutarch.Lift (PConstantDecl, PLifted, PUnsafeLiftDecl (..))
import Plutarch.Prelude
import Plutus.MerkleTree (Hash)
import "liqwid-plutarch-extra" Plutarch.Extra.List (pisSingleton)
import "plutarch-extra" Plutarch.Extra.List (preverse)

type PProof = PBuiltinList (PAsData (PEitherData PHash PHash))

newtype PHash (s :: S) = PHash (Term s (PDataRecord '["_0" ':= PByteString]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, PPartialOrd, POrd, PShow)

instance DerivePlutusType PHash where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PHash where type PLifted PHash = Hash

deriving via (DerivePConstantViaData Hash PHash) instance PConstantDecl Hash

instance PTryFrom PData (PAsData PHash)

instance PTryFrom PData PHash

instance Semigroup (Term s PHash) where
  x' <> y' =
    phoistAcyclic
      ( plam $ \x y ->
          pcon $ PHash $ pdcons # pdata (pfield @"_0" # x <> pfield @"_0" # y) # pdnil
      )
      # x'
      # y'

instance Monoid (Term s PHash) where
  mempty = pcon $ PHash $ pdcons # pdata mempty # pdnil

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

_pdrop :: (PIsListLike list a) => Term s PInteger -> Term s (list a) -> Term s (list a)
_pdrop n xs = pdrop' # n # xs
  where
    pdrop' :: (PIsListLike list a) => Term s (PInteger :--> list a :--> list a)
    pdrop' = pfix #$ plam $ \self i ls ->
      pif
        (i #== 0)
        ls
        $ pif
          (i #== 1)
          (ptail # ls)
        $ self # (i - 1) # (ptail # ls)

_pfromList :: forall {s :: S}. Term s (PBuiltinList PByteString :--> PMerkleTree)
_pfromList = phoistAcyclic $
  plam $ \x ->
    let go = pfix #$ plam $ \_self len list ->
          pmatch list $ \case
            PNil -> pcon PMerkleEmpty
            ls ->
              pif
                (pisSingleton #$ pcon ls)
                (plet (phead #$ pcon ls) $ \v -> pcon $ PMerkleLeaf (phash # v) v)
                ( plet (pdiv # len # 2) $ \_cutoff ->
                    let _left = preverse #$ _pdrop _cutoff $ preverse # pcon ls
                        right = _pdrop _cutoff $ pcon ls
                        leftnode = _self # _cutoff # _left
                        rightnode = _self # _cutoff # right
                     in pcon $ PMerkleNode (prootHash # leftnode <> prootHash # rightnode) leftnode rightnode
                )
     in go # (plength # x) # x

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

-- pmkProof :: forall (s :: S). Term s (PByteString :--> PMerkleTree :--> PMaybe PProof)
-- pmkProof = phoistAcyclic $
--   plam $ \bs -> plet (phash # bs) $ \he ->
--     let go = pfix #$ plam $ \self es merkT ->
--           pmatch merkT $ \case
--             PMerkleEmpty -> pcon PNothing
--             PMerkleLeaf h _ ->
--               pif
--                 (h #== he)
--                 (pcon $ PJust es)
--                 (pcon PNothing)
--             PMerkleNode _ l r ->
--               (self # pcon (PCons (pcon $ PDRight (pdcons @"_0" # pdata (prootHash # r) # pdnil)) es) # l)
--                 #<!> (self # pcon (PCons (pcon $ PDLeft (pdcons @"_0" # pdata (prootHash # l) # pdnil)) es) # r)
--      in go # pcon PNil

pmember :: forall (s :: S). Term s (PByteString :--> PHash :--> PProof :--> PBool)
pmember = phoistAcyclic $ plam $ \bs root proof ->
  let go = pfix #$ plam $ \self root' ls' ->
        pmatch ls' $ \case
          PNil -> root' #== root
          PCons x xs ->
            pmatch (pfromData x) $ \case
              PDLeft l ->
                plet (pfield @"_0" # l) $ \l' ->
                  self # (pcombineHash # l' # root') # xs
              PDRight r ->
                plet (pfield @"_0" # r) $ \r' ->
                  self # (pcombineHash # root' # r') # xs
   in go # (phash # bs) # proof

phash :: forall (s :: S). Term s (PByteString :--> PHash)
phash = phoistAcyclic $ plam $ \bs ->
  pcon $ PHash $ pdcons # pdata (psha2_256 # bs) # pdnil

pcombineHash :: Term s (PHash :--> PHash :--> PHash)
pcombineHash = phoistAcyclic $
  plam $ \h1 h2 -> unTermCont $ do
    pure $
      phash # (pfield @"_0" # h1 <> pfield @"_0" # h2)

-- mt :: forall {s :: S}. Term s PMerkleTree
-- mt = pcon $ PMerkleLeaf (phash #$ phexByteStr "41") (phexByteStr "41")

-- proof :: forall {s :: S}. Term s PProof
-- proof = pfromJust #$ pmkProof # phexByteStr "41" # mt

-- rh :: forall {s :: S}. Term s PHash
-- rh = prootHash # mt

-- isMember :: forall {s :: S}. Term s PBool
-- isMember = pmember # phexByteStr "41" # rh # proof
