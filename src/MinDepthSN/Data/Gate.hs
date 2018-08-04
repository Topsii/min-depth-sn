{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# language PatternSynonyms #-}

module MinDepthSN.Data.Gate
    ( Gate(Gate)
    ) where

import Data.Ord (comparing)
import Data.Monoid ((<>))
import Data.Map.Strict (Map)
import Data.Array (Array)
import GHC.Generics (Generic)
import Enumerate (Enumerable)
import Enumerate.Enum (toEnum_enumerable, fromEnum_enumerable)
import Enumerate.Enum.Valid (Validatable, isValid, tableEnumerable, arrayEnumerable, validMaxBound, validMinBound)
import MinDepthSN.Data.Size

data Gate = ComparatorGate { minChannel :: Channel, maxChannel :: Channel, layer :: Layer }
    deriving (Eq, Generic, Enumerable)

pattern Gate :: Channel -> Channel -> Layer -> Gate
pattern Gate i j k <- ComparatorGate i j k where
    Gate i j k
        | i < j     = ComparatorGate i j k
        | otherwise = error $ "Gate i j k: Requires i < j, but given was " 
            ++ show i ++ " = i >= j = " ++ show j

instance Show Gate where
    show (ComparatorGate i j k) = "Gate " ++ show i ++ " " ++ show j ++ " " ++ show k

instance Ord Gate where
    compare = comparing layer <> comparing minChannel <> comparing maxChannel

instance Validatable Gate where
    isValid (ComparatorGate i j _)
        | i < j = True
        | otherwise = False

instance Bounded Gate where
    minBound = validMinBound
    maxBound = validMaxBound

instance Enum Gate where
    toEnum   = toEnum_enumerable   arrayGate
    fromEnum = fromEnum_enumerable tableGate

tableGate :: Map Gate Int
tableGate = tableEnumerable

arrayGate :: Array Int Gate
arrayGate = arrayEnumerable