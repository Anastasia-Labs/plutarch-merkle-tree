{-# LANGUAGE TemplateHaskell #-}

module Spec.MerkleTreeSpec (myMerkleTree, myRootHash, myProof, myDatum, myRedeemer, myProof2, myRedeemer2, scriptEvaluation, scriptEvaluation2) where

import Data.Maybe (fromJust)
import Data.Text (Text)
import Plutarch (Script)
import Plutarch.Api.V2 (PValidator)
import Plutarch.DataRepr (DerivePConstantViaData (..), PDataFields)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (pletC, pletFieldsC, ptraceC, ptryFromC)
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import Plutarch.MerkleTree (Hash, MerkleTree, PHash, PProof, Proof, fromList, mkProof, pmember, rootHash)
import Plutarch.Prelude
import PlutusLedgerApi.V2 (ExBudget)
import PlutusTx qualified
import PlutusTx.Builtins (BuiltinByteString)
import PlutusTx.Prelude (encodeUtf8)
import Utils (evalWithArgsT)

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

myProof :: Proof
myProof = fromJust $ mkProof (encodeUtf8 "2") myMerkleTree

myDatum :: MyDatum
myDatum = MyDatum myRootHash

myRedeemer :: MyRedeemer
myRedeemer = MyRedeemer myProof (encodeUtf8 "2")

myProof2 :: Proof
myProof2 = fromJust $ mkProof (encodeUtf8 "1") myMerkleTree

myRedeemer2 :: MyRedeemer
myRedeemer2 = MyRedeemer myProof2 (encodeUtf8 "1")

scriptEvaluation :: Either Text (Script, ExBudget, [Text])
scriptEvaluation = evalWithArgsT validator [PlutusTx.toData myDatum, PlutusTx.toData myRedeemer, PlutusTx.toData ()]

scriptEvaluation2 :: Either Text (Script, ExBudget, [Text])
scriptEvaluation2 = evalWithArgsT validator [PlutusTx.toData myDatum, PlutusTx.toData myRedeemer2, PlutusTx.toData ()]
