{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# language PatternSynonyms #-}

module MinDepthSN.Data.GeneralizedGate
    ( GeneralizedGate(GeneralizedGate)
    ) where

import Data.Ord (comparing)
import Data.Monoid ((<>))
import Data.Map.Strict (Map)
import Data.Array (Array)
import GHC.Generics (Generic)
import Enumerate (Enumerable)
import Enumerate.Enum (toEnum_enumerable, fromEnum_enumerable)
import Enumerate.Enum.Valid
    ( Validatable
    , isValid
    , tableEnumerable
    , arrayEnumerable
    , validMaxBound
    , validMinBound
    )
import MinDepthSN.Data.Size (Channel, Layer)

data GeneralizedGate = ComparatorGate {
    minChannel :: Channel,
    maxChannel :: Channel,
    layer :: Layer 
} deriving (Eq, Generic, Enumerable)

pattern GeneralizedGate :: Channel -> Channel -> Layer -> GeneralizedGate
pattern GeneralizedGate i j k <- ComparatorGate i j k where
    GeneralizedGate i j k
        | i /= j     = ComparatorGate i j k
        | otherwise = error $ "GeneralizedGate i j k: Requires i /= j, but "
            ++ "given was " ++ show i ++ " = i = j = " ++ show j

instance Show GeneralizedGate where
    show (ComparatorGate i j k) = 
        "GeneralizedGate " ++ show i ++ " " ++ show j ++ " " ++ show k

instance Ord GeneralizedGate where
    compare = comparing layer <> comparing minChannel <> comparing maxChannel

instance Validatable GeneralizedGate where
    isValid (ComparatorGate i j _)
        | i /= j = True
        | otherwise = False

instance Bounded GeneralizedGate where
    minBound = validMinBound
    maxBound = validMaxBound

instance Enum GeneralizedGate where
    toEnum   = toEnum_enumerable   arrayGate
    fromEnum = fromEnum_enumerable tableGate

tableGate :: Map GeneralizedGate Int
tableGate = tableEnumerable

arrayGate :: Array Int GeneralizedGate
arrayGate = arrayEnumerable