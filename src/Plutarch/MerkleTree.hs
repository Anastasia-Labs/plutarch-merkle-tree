{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Plutarch.MerkleTree (
  validator,
  PHash (PHash),
  PMerkleTree (..),
  phash,
  -- pmember,
  -- pmkProof,
  -- proof,
  -- isMember,
  _pdrop,
  Proof,
  fromList,
  toList,
  isNull,
  size,
  mkProof,
  member,
  scriptEvaluation,
  myProof,
  myMerkleTree,
)
where

import Control.Applicative ((<|>))
import Data.Maybe (fromJust)
import Plutarch.Api.V2 (
  PValidator,
 )
import Plutarch.DataRepr (DerivePConstantViaData (DerivePConstantViaData), PDataFields)
import "liqwid-plutarch-extra" Plutarch.Extra.List (pisSingleton)
import "plutarch-extra" Plutarch.Extra.List (preverse)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (pletC, pletFieldsC, pmatchC, ptraceC, ptryFromC)
import Plutarch.Lift (DerivePConstantViaBuiltin (DerivePConstantViaBuiltin), DerivePConstantViaNewtype (..), PConstantDecl (PConstanted), PLifted, PUnsafeLiftDecl (..))
import Plutarch.Prelude
import PlutusTx qualified
import PlutusTx.Builtins (BuiltinByteString, appendByteString, divideInteger, sha2_256)
import PlutusTx.Foldable qualified
import PlutusTx.Prelude qualified
import Utils (evalWithArgsT)

-- PEitherData for Data type, similar to PMaybeData

data PEitherData a b (s :: S)
  = PDLeft (Term s (PDataRecord '["_0" ':= a]))
  | PDRight (Term s (PDataRecord '["_0" ':= b]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType (PEitherData a b) where type DPTStrat _ = PlutusTypeData

instance (PLiftData a, PLiftData b) => PUnsafeLiftDecl (PEitherData a b) where type PLifted (PEitherData a b) = Either (PLifted a) (PLifted b)

deriving via (DerivePConstantViaData (Either a b) (PEitherData (PConstanted a) (PConstanted b))) instance (PConstantData a, PConstantData b) => PConstantDecl (Either a b)

instance (PTryFrom PData a, PTryFrom PData b) => PTryFrom PData (PEitherData a b)

instance (PTryFrom PData a, PTryFrom PData b) => PTryFrom PData (PAsData (PEitherData a b))

-- Hash and PHash

newtype Hash = Hash BuiltinByteString
  deriving stock (Show, Eq, Ord)

PlutusTx.makeIsDataIndexed ''Hash [('Hash, 0)]

-- FIXME: Implement Data Record instead
newtype PHash (s :: S) = PHash (Term s PByteString)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PPartialOrd, POrd, PShow)

instance DerivePlutusType PHash where type DPTStrat _ = PlutusTypeNewtype

instance PUnsafeLiftDecl PHash where type PLifted PHash = Hash

deriving via (DerivePConstantViaBuiltin Hash PHash PByteString) instance PConstantDecl Hash

instance PTryFrom PData (PAsData PHash)

-- instance PTryFrom PData (PAsData PHash) where
--   type PTryFromExcess PData (PAsData PHash) = Flip Term PHash
--   ptryFrom' opq = runTermCont $ do
--     unwrapped <- tcont . plet $ ptryFrom @(PAsData PByteString) opq snd
--     tcont $ \f ->
--       pif (plengthBS # unwrapped #<= 32) (f ()) (ptraceError "ptryFrom(PHash): must be at most 32 Bytes long")
--     pure (punsafeCoerce opq, pcon . PHash $ unwrapped)
--
-- newtype Flip f a b = Flip (f b a) deriving stock (Generic)

-- type Proof = [Either Hash Hash]
newtype Proof = Proof [Either Hash Hash]
  deriving stock (Show, Eq, Ord)

PlutusTx.makeIsDataIndexed ''Proof [('Proof, 0)]

-- type PProof = PBuiltinList (PEitherData PHash PHash)
-- type PProof = PBuiltinList (PAsData (PEitherData (PAsData PHash) (PAsData PHash)))
newtype PProof (s :: S)
  = PProof
      ( Term
          s
          (PBuiltinList (PEitherData PHash PHash))
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow)

instance DerivePlutusType PProof where type DPTStrat _ = PlutusTypeNewtype

instance PUnsafeLiftDecl PProof where type PLifted PProof = Proof

deriving via (DerivePConstantViaNewtype Proof PProof (PBuiltinList (PEitherData PHash PHash))) instance (PConstantDecl Proof)

instance PTryFrom PData (PAsData (PBuiltinList (PEitherData PHash PHash)))

instance PTryFrom PData (PAsData PProof)

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

-- pmember :: forall (s :: S). Term s (PByteString :--> PHash :--> PProof :--> PBool)
-- pmember = phoistAcyclic $
--   plam $ \bs root ->
--     let go = pfix #$ plam $ \self root' proof ->
--           pmatch ( proof ) $ \case
--             PNil -> root' #== root
--             PCons x xs -> plet (pfromData x) $ \x' ->
--               pmatch x' $ \case
--                 PDLeft l ->
--                   plet (pfield @"_0" # l) $ \l' ->
--                     plet (punsafeCoerce l' :: Term _ (PAsData PHash)) $ \lh ->
--                       self # (pcombineHash # pfromData lh # root') # xs
--                 PDRight r ->
--                   plet (pfield @"_0" # r) $ \r' ->
--                     plet (punsafeCoerce r' :: Term _ (PAsData PHash)) $ \rh ->
--                       self # (pcombineHash # pfromData rh # root') # xs
--      in go # (phash # bs)

pmember :: forall (s :: S). Term s (PByteString :--> PHash :--> PProof :--> PBool)
pmember = phoistAcyclic $ plam $ \bs root proof -> pcon PTrue

-- pmatch proof $ \case
--   PProof ls ->
--     let go = pfix #$ plam $ \self root' ls ->
--           pmatch ls $ \case
--             PNil ->
--               root' #== root
--             PCons x xs ->
--               pmatch x $ \case
--                 PDLeft l ->
--                   plet (pfield @"_0" # l) $ \l' ->
--                     self # (pcombineHash # l' # root') # xs
--                 PDRight l ->
--                   plet (pfield @"_0" # l) $ \l' ->
--                     self # (pcombineHash # l' # root') # xs
--      in go # (phash # bs) # ls

phash :: forall (s :: S). Term s (PByteString :--> PHash)
phash = phoistAcyclic $ plam $ \bs ->
  pcon $ PHash (psha2_256 # bs)

pcombineHash :: Term s (PHash :--> PHash :--> PHash)
pcombineHash = phoistAcyclic $
  plam $ \h1 h2 -> unTermCont $ do
    PHash h1' <- pmatchC h1
    PHash h2' <- pmatchC h2
    pure $ phash # (h1' <> h2')

-- mt :: forall {s :: S}. Term s PMerkleTree
-- mt = pcon $ PMerkleLeaf (phash #$ phexByteStr "41") (phexByteStr "41")

-- proof :: forall {s :: S}. Term s PProof
-- proof = pfromJust #$ pmkProof # phexByteStr "41" # mt

-- rh :: forall {s :: S}. Term s PHash
-- rh = prootHash # mt

-- isMember :: forall {s :: S}. Term s PBool
-- isMember = pmember # phexByteStr "41" # rh # proof

-- Haskell types

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
          then Just (Proof es)
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
    go root' (Proof p) = case p of
      [] -> root' == root
      (Left l) : q -> go (combineHash l root') (Proof q)
      (Right r) : q -> go (combineHash root' r) (Proof q)
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

-- Validator Test
newtype MyDatum = MyDatum {merkleRoot :: Hash}
  deriving stock (Generic, Eq, Show)
  deriving newtype (PlutusTx.ToData, PlutusTx.FromData)

newtype PMyDatum (s :: S)
  = PMyDatum
      ( Term
          s
          ( PDataRecord
              '[ "mekleRootHash" ':= PHash
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow)

instance DerivePlutusType PMyDatum where type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PMyDatum

instance PUnsafeLiftDecl PMyDatum where type PLifted PMyDatum = MyDatum

deriving via (DerivePConstantViaData MyDatum PMyDatum) instance (PConstantDecl MyDatum)

data MyRedeemer = MyRedeemer {myProof :: Proof, userData :: BuiltinByteString}
  deriving stock (Generic, Eq, Show)

PlutusTx.makeIsDataIndexed ''MyRedeemer [('MyRedeemer, 0)]

newtype PMyRedeemer (s :: S)
  = PMyRedeemer
      ( Term
          s
          ( PDataRecord
              '[ "myProof" ':= PProof
               , "userData" ':= PByteString
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow)

instance DerivePlutusType PMyRedeemer where type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PMyRedeemer

instance PUnsafeLiftDecl PMyRedeemer where type PLifted PMyRedeemer = MyRedeemer

deriving via (DerivePConstantViaData MyRedeemer PMyRedeemer) instance (PConstantDecl MyRedeemer)

validator :: ClosedTerm PValidator
validator = plam $ \d r _ -> unTermCont $ do
  dat <- fst <$> ptryFromC @PMyDatum d
  merkleRoot <- pletC $ pfield @"mekleRootHash" # dat
  myRed <- fst <$> ptryFromC @PMyRedeemer r
  myRedF <- pletFieldsC @["myProof", "userData"] myRed
  PProof ls <- pmatchC $ pfromData myRedF.myProof
  ptraceC $ pshow ls
  ptraceC $ pshow (myRedF.userData)
  ptraceC $ pshow merkleRoot
  -- isValid <- pletC $ pmember # myRedF.userData # merkleRoot # myRedF.myProof
  pure $
    popaque $
      pif
        -- isValid
        (pconstant True)
        (pconstant ())
        perror

-- Validator unit test
myMerkleTree :: MerkleTree
myMerkleTree = fromList $ PlutusTx.Prelude.encodeUtf8 <$> ["1", "2", "3"]

myRootHash :: Hash
myRootHash = rootHash myMerkleTree

myProof :: Proof
myProof = fromJust $ mkProof (PlutusTx.Prelude.encodeUtf8 "2") myMerkleTree

myDatum :: MyDatum
myDatum = MyDatum myRootHash

myRedeemer :: MyRedeemer
myRedeemer = MyRedeemer myProof (PlutusTx.Prelude.encodeUtf8 "2")

scriptEvaluation = evalWithArgsT validator [PlutusTx.toData myDatum, PlutusTx.toData myRedeemer, PlutusTx.toData ()]

--
-- type PProof' = PBuiltinList (PAsData (PEitherData PData PData))
--
-- validator :: ClosedTerm PValidator
-- validator = plam $ \_ r _ -> unTermCont $ do
--   _test <- pfromData . fst <$> ptryFromC @(PAsData PProof') r
--   let _x =
--         precList
--           ( \self x xs -> plet (pfromData x) $ \x' ->
--               pmatch x' $ \case
--                 PDLeft l ->
--                   plet (pfromData $ pfield @"_0" # l) $ \l' ->
--                     ptryFrom @(PAsData PHash) l' $ \(l'', _) ->
--                       ptrace ( pshow l'') (self # xs)
--                 PDRight r ->
--                   plet (pfromData $ pfield @"_0" # r) $ \r' ->
--                     ptryFrom @(PAsData PHash) r' $ \(r'', _) ->
--                       ptrace ( pshow r'') (self # xs)
--           )
--           (const _test)
--           #$ _test
--
--   pure $ popaque $ pconstant True
