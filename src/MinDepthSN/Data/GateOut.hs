{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}

module MinDepthSN.Data.GateOut where

import Data.Ord (comparing)
import Data.Monoid ((<>))
import Data.Map.Strict (Map)
import Data.Array (Array)
import MinDepthSN.Data.Size (Gate, Layer)
import GHC.Generics (Generic)
import Enumerate (Enumerable)
import Enumerate.Enum (toEnum_enumerable, fromEnum_enumerable)
import Enumerate.Enum.Valid (Validatable, isValid, tableEnumerable, arrayEnumerable,  validMaxBound, validMinBound)

data GateOut = GateOut {
    minForwardGate :: Gate,
    maxForwardGate :: Gate,
    gate :: Gate,
    minForwardLayer :: Layer,
    maxForwardLayer :: Layer,
    layer :: Layer
} deriving (Eq, Generic, Enumerable, Show)

instance Ord GateOut where
    compare = 
        comparing layer <> 
        comparing minForwardLayer <> 
        comparing maxForwardLayer <> 
        comparing gate <> 
        comparing minForwardGate <> 
        comparing maxForwardGate

instance Validatable GateOut where
    isValid go = layer go < minForwardLayer go && layer go < maxForwardLayer go

instance Bounded GateOut where
    minBound = validMinBound
    maxBound = validMaxBound

instance Enum GateOut where
    toEnum   = toEnum_enumerable   arrayGateOut
    fromEnum = fromEnum_enumerable tableGateOut

tableGateOut :: Map GateOut Int
tableGateOut = tableEnumerable

arrayGateOut :: Array Int GateOut
arrayGateOut = arrayEnumerable