{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MinDepthSN.Data.GateOut where

import Data.Ord (comparing)
import Data.Monoid ((<>))
import MinDepthSN.Data.Size (Gate, Layer)
import GHC.Generics (Generic)
import Enumerate
    ( Enumerable
    , enumerated
    , boundedEnumerated
    , cardinality
    , boundedCardinality
    )
import Enumerate.Enum.Valid (Validatable, Valid, isValid)

data ComparatorGateOut = ComparatorGateOut {
    minForwardGate :: Gate,
    maxForwardGate :: Gate,
    gate :: Gate,
    minForwardLayer :: Layer,
    maxForwardLayer :: Layer,
    layer :: Layer
} deriving (Eq, Generic, Enumerable, Show)

instance Ord ComparatorGateOut where
    compare = 
        comparing layer <> 
        comparing minForwardLayer <> 
        comparing maxForwardLayer <> 
        comparing gate <> 
        comparing minForwardGate <> 
        comparing maxForwardGate

instance Validatable ComparatorGateOut where
    isValid go = layer go < minForwardLayer go && layer go < maxForwardLayer go

newtype GateOut = ValidGateOut (Valid ComparatorGateOut)
    deriving newtype (Eq, Ord, Validatable, Bounded, Enum)

instance Enumerable GateOut where
    enumerated = boundedEnumerated
    cardinality = boundedCardinality
