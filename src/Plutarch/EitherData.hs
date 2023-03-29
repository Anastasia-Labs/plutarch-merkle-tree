{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.EitherData (PEitherData (PDLeft, PDRight)) where

import Plutarch.DataRepr (DerivePConstantViaData (DerivePConstantViaData))
import Plutarch.Lift (PConstantDecl (PConstanted), PUnsafeLiftDecl (PLifted))
import Plutarch.Prelude

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
