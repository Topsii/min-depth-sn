{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module MinDepthSN.Data.GateOut
    ( GateOut(GateOut)
    ) where

import Data.Ord (comparing)
import Data.Monoid ((<>))
import MinDepthSN.Data.Size (GateInLayer, Layer)
import GHC.Generics (Generic)
import Enumerate
    ( Enumerable
    , enumerated
    , boundedEnumerated
    , cardinality
    , boundedCardinality
    )
import Enumerate.Enum.Valid (Validatable, Valid(..), isValid)

data UnvalidatedGateOut = UnvalidatedGateOut {
    minForwardGate :: GateInLayer,
    maxForwardGate :: GateInLayer,
    gate :: GateInLayer,
    minForwardLayer :: Layer,
    maxForwardLayer :: Layer,
    layer :: Layer
} deriving (Eq, Generic, Enumerable, Show)

instance Ord UnvalidatedGateOut where
    compare = 
        comparing layer <> 
        comparing minForwardLayer <> 
        comparing maxForwardLayer <> 
        comparing gate <> 
        comparing minForwardGate <> 
        comparing maxForwardGate


instance Validatable UnvalidatedGateOut where
    isValid UnvalidatedGateOut {..} =
        layer < minForwardLayer && layer < maxForwardLayer

newtype GateOut = ValidGateOut (Valid UnvalidatedGateOut)
    deriving newtype (Eq, Ord, Validatable, Bounded, Enum)

pattern GateOut :: GateInLayer -> GateInLayer -> GateInLayer -> Layer -> Layer -> Layer -> GateOut
pattern GateOut a b c d e f = ValidGateOut (Valid (UnvalidatedGateOut a b c d e f))

instance Enumerable GateOut where
    enumerated = boundedEnumerated
    cardinality = boundedCardinality
