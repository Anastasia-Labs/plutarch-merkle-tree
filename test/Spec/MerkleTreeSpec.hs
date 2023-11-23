{-# LANGUAGE TemplateHaskell #-}

module Spec.MerkleTreeSpec (
  myMerkleTree,
  myRootHash,
  myDatum,
  goodProof2,
  goodRedeemer2,
  unitTest,
)
where

import Data.Maybe (fromJust)
import Plutarch.Api.V2 (PValidator)
import Plutarch.DataRepr (DerivePConstantViaData (DerivePConstantViaData), PDataFields)
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import Plutarch.MerkleTree (PHash, PProof, pmember)
import Plutarch.Prelude
import Plutarch.Test.Precompiled (Expectation (Failure, Success), testEvalCase, tryFromPTerm)
import Plutus.MerkleTree (Hash, MerkleTree, Proof, fromList, mkProof, rootHash)
import PlutusTx qualified
import PlutusTx.Builtins (BuiltinByteString)
import PlutusTx.Prelude (encodeUtf8)
import Test.Tasty (TestTree)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (pletC, pletFieldsC, ptraceC, ptryFromC)

-- Validator Test
newtype MyDatum = MyDatum {merkleRoot :: Hash}
  deriving stock (Generic, Eq, Show)

PlutusTx.makeIsDataIndexed ''MyDatum [('MyDatum, 0)]

{- | Data type representing the datum of the smart contract.
In Plutus smart contracts, the datum provides essential
information about the state or context of the transaction.
Here, it includes the Merkle Tree root hash.
-}
newtype PMyDatum (s :: S)
  = PMyDatum
      ( Term
          s
          ( PDataRecord
              '[ "mekleRootHash" ':= PHash -- \^ The root hash of the Merkle Tree.
              -- This hash represents the state of the data set
              -- that the Merkle Tree is built from.
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

{- | Data type representing the redeemer of the smart contract.
The redeemer is used in Plutus smart contracts to provide
context or arguments for transaction validation.
In this case, it contains a Merkle Tree proof and user data.
-}
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

{- | The validator serves as the core logic of the contract,
determining whether a given transaction is valid based on
Merkle Tree proofs and the associated data.
-}
validator :: ClosedTerm PValidator
validator = plam $ \d r _ -> unTermCont $ do
  -- Extracts MyDatum from the transaction's datum and retrieves the Merkle Root Hash.
  dat <- fst <$> ptryFromC @PMyDatum d
  merkleRoot <- pletC $ pfield @"mekleRootHash" # dat

  -- Extracts MyRedeemer from the transaction's redeemer and retrieves the proof and user data.
  myRed <- fst <$> ptryFromC @PMyRedeemer r
  myRedF <- pletFieldsC @["myProof", "userData"] myRed

  -- Debug: Prints the proof for logging and debugging purposes.
  ptraceC $ pshow myRedF.myProof

  -- Checks if the user data is a valid member of the Merkle Tree using the provided proof.
  isValid <- pletC $ pmember # myRedF.userData # merkleRoot # myRedF.myProof

  -- Debug: Prints the result of the validity check.
  ptraceC $ pshow isValid

  -- Returns a constant unit type if the transaction is valid, or raises an error otherwise.
  -- This is the crux of the validation logic, determining whether to approve or reject the transaction.
  pure $
    popaque $
      pif
        isValid
        (pconstant ())
        perror

{- | Merkle tree setup for testing.
Constructs a Merkle Tree from a list of byte strings.
This simulates a real-world data structure for the contract.
-}
myMerkleTree :: MerkleTree
myMerkleTree = fromList $ encodeUtf8 <$> ["1", "2", "3"]

{- | Root hash of the Merkle Tree.
Represents the integrity of the tree and is used in myDatum
-}
myRootHash :: Hash
myRootHash = rootHash myMerkleTree

-- | Datum for the test, encapsulating the root hash of the Merkle Tree.
myDatum :: MyDatum
myDatum = MyDatum myRootHash

goodProof1 :: Proof
goodProof1 = fromJust $ mkProof "1" myMerkleTree

goodRedeemer1 :: MyRedeemer
goodRedeemer1 = MyRedeemer goodProof1 "1"

goodProof2 :: Proof
goodProof2 = fromJust $ mkProof "2" myMerkleTree

goodRedeemer2 :: MyRedeemer
goodRedeemer2 = MyRedeemer goodProof2 "2"

goodProof3 :: Proof
goodProof3 = fromJust $ mkProof "3" myMerkleTree

goodRedeemer3 :: MyRedeemer
goodRedeemer3 = MyRedeemer goodProof3 "3"

badRedeemer4 :: MyRedeemer
badRedeemer4 = MyRedeemer goodProof1 "4"

{- | Validator unit tests.
Tests the validator's ability to handle valid and invalid scenarios
ensuring the integrity and security of transactions.
-}
unitTest :: TestTree
unitTest = tryFromPTerm "Merkle Tree Unit Test" validator $ do
  -- Tests validator's approval for a transaction with a valid proof for an existing member ("1")
  testEvalCase
    "Pass - Validation of member 1"
    Success
    [ PlutusTx.toData myDatum
    , PlutusTx.toData goodRedeemer1
    , PlutusTx.toData ()
    ]
  -- Similar test for another valid member ("2")
  testEvalCase
    "Pass - Validation of member 2"
    Success
    [ PlutusTx.toData myDatum
    , PlutusTx.toData goodRedeemer2
    , PlutusTx.toData ()
    ]
  -- Test for another valid member ("3")
  testEvalCase
    "Pass - Validation of member 3"
    Success
    [ PlutusTx.toData myDatum
    , PlutusTx.toData goodRedeemer3
    , PlutusTx.toData ()
    ]
  -- Tests validator's rejection of a transaction with an invalid proof for a non-existent member ("4")
  testEvalCase
    "Fail - Validation of member 4"
    Failure
    [ PlutusTx.toData myDatum
    , PlutusTx.toData badRedeemer4
    , PlutusTx.toData ()
    ]
