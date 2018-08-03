
{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}

module MinDepthSN.Data.Value
    ( Value(Value)
    , inputValues
    , outputValues
    , firstInputValue
    , lastInputValue
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

data Value = Value { channel :: Channel, betweenLayers :: BetweenLayers }
    deriving (Eq, Generic, Enumerable)

instance Show Value where
    show (Value i k) = "Val " ++ show i ++ " " ++ show k

instance Ord Value where
    compare = comparing betweenLayers <> comparing channel

instance Bounded Value where
    minBound = validMinBound
    maxBound = validMaxBound

instance Validatable Value where
    isValid = const True

instance Enum Value where
    toEnum   = toEnum_enumerable   arrayValue
    fromEnum = fromEnum_enumerable tableValue

tableValue :: Map Value Int
tableValue = tableEnumerable

arrayValue :: Array Int Value
arrayValue = arrayEnumerable

inputValues :: [Value]
inputValues = [ Value i beforeFirstLayer | i <- channels ]

outputValues :: [Value]
outputValues = [ Value i afterLastLayer | i <- channels ]

firstInputValue :: Value
firstInputValue = head inputValues

lastInputValue :: Value
lastInputValue = last inputValues