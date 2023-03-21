{-# LANGUAGE TemplateHaskell #-}

module Spec.MerkleTreeSpec (myMerkleTree, myRootHash, myDatum, goodProof2, goodRedeemer2, unitTest) where

import Data.Maybe (fromJust)
import Plutarch.Api.V2 (PValidator)
import Plutarch.DataRepr (DerivePConstantViaData (..), PDataFields)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (pletC, pletFieldsC, ptraceC, ptryFromC)
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import Plutarch.MerkleTree (Hash, MerkleTree, PHash, PProof, Proof, fromList, mkProof, pmember, rootHash)
import Plutarch.Prelude
import Plutarch.Test.Precompiled (Expectation (Failure, Success), testEvalCase, tryFromPTerm)
import PlutusTx qualified
import PlutusTx.Builtins (BuiltinByteString)
import PlutusTx.Prelude (encodeUtf8)
import Test.Tasty (TestTree)

-- Validator Test
newtype MyDatum = MyDatum {merkleRoot :: Hash}
  deriving stock (Generic, Eq, Show)

PlutusTx.makeIsDataIndexed ''MyDatum [('MyDatum, 0)]

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

instance PTryFrom PData (PAsData PMyDatum)

instance PUnsafeLiftDecl PMyDatum where type PLifted PMyDatum = MyDatum

deriving via (DerivePConstantViaData MyDatum PMyDatum) instance (PConstantDecl MyDatum)

data MyRedeemer = MyRedeemer {goodProof :: Proof, userData :: BuiltinByteString}
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
  ptraceC $ pshow myRedF.myProof
  isValid <- pletC $ pmember # myRedF.userData # merkleRoot # myRedF.myProof
  ptraceC $ pshow isValid
  pure $
    popaque $
      pif
        isValid
        (pconstant ())
        perror

-- Validator unit test
myMerkleTree :: MerkleTree
myMerkleTree = fromList $ encodeUtf8 <$> ["1", "2", "3"]

myRootHash :: Hash
myRootHash = rootHash myMerkleTree

myDatum :: MyDatum
myDatum = MyDatum myRootHash

goodProof1 :: Proof
goodProof1 = fromJust $ mkProof (encodeUtf8 "1") myMerkleTree

goodRedeemer1 :: MyRedeemer
goodRedeemer1 = MyRedeemer goodProof1 (encodeUtf8 "1")

goodProof2 :: Proof
goodProof2 = fromJust $ mkProof (encodeUtf8 "2") myMerkleTree

goodRedeemer2 :: MyRedeemer
goodRedeemer2 = MyRedeemer goodProof2 (encodeUtf8 "2")

goodProof3 :: Proof
goodProof3 = fromJust $ mkProof (encodeUtf8 "3") myMerkleTree

goodRedeemer3 :: MyRedeemer
goodRedeemer3 = MyRedeemer goodProof3 (encodeUtf8 "3")

badRedeemer4 :: MyRedeemer
badRedeemer4 = MyRedeemer goodProof1 (encodeUtf8 "4")

unitTest :: TestTree
unitTest = tryFromPTerm "Merkle Tree Unit Test" validator $ do
  testEvalCase
    "Pass - Validation of member 1"
    Success
    [ PlutusTx.toData myDatum
    , PlutusTx.toData goodRedeemer1
    , PlutusTx.toData ()
    ]
  testEvalCase
    "Pass - Validation of member 2"
    Success
    [ PlutusTx.toData myDatum
    , PlutusTx.toData goodRedeemer2
    , PlutusTx.toData ()
    ]
  testEvalCase
    "Pass - Validation of member 3"
    Success
    [ PlutusTx.toData myDatum
    , PlutusTx.toData goodRedeemer3
    , PlutusTx.toData ()
    ]
  testEvalCase
    "Fail - Validation of member 4"
    Failure
    [ PlutusTx.toData myDatum
    , PlutusTx.toData badRedeemer4
    , PlutusTx.toData ()
    ]
