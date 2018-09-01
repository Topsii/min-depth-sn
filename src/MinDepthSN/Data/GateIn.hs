{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module MinDepthSN.Data.GateIn
    ( GateIn(GateIn)
    ) where

import Data.Ord (comparing)
import Data.Monoid ((<>))
import MinDepthSN.Data.Size
import GHC.Generics (Generic)
import Enumerate
    ( Enumerable
    , enumerated
    , boundedEnumerated
    , cardinality
    , boundedCardinality
    )
import Enumerate.Enum.Valid (Validatable, Valid(..), isValid)

data UnvalidatedGateIn = UnvalidatedGateIn {
    inboundChannel1 :: Channel,
    inboundChannel2 :: Channel, -- inboundChannel1 < inboundChannel2
    gate :: GateInLayer,
    inboundLayer1 :: Layer,
    inboundLayer2 :: Layer,
    layer :: Layer
} deriving (Eq, Generic, Enumerable, Show)

instance Ord UnvalidatedGateIn where
    compare = 
        comparing layer <> 
        comparing inboundLayer1 <> 
        comparing inboundLayer2 <> 
        comparing gate <> 
        comparing inboundChannel1 <> 
        comparing inboundChannel2

instance Validatable UnvalidatedGateIn where
    isValid UnvalidatedGateIn {..} =
        inboundChannel1 < inboundChannel2
        && layer == 1 + max inboundLayer1 inboundLayer2
        && (even n || inboundChannel2 /= maxBound || inboundLayer2 == minBound)

newtype GateIn = ValidGateIn (Valid UnvalidatedGateIn)
    deriving newtype (Eq, Ord, Validatable, Bounded, Enum)

pattern GateIn :: Channel -> Channel -> GateInLayer -> Layer -> Layer -> Layer -> GateIn
pattern GateIn a b c d e f = ValidGateIn (Valid (UnvalidatedGateIn a b c d e f))

instance Enumerable GateIn where
    enumerated = boundedEnumerated
    cardinality = boundedCardinality
