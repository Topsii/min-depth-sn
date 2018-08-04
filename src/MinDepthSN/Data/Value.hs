
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
import GHC.Generics (Generic)
import Enumerate (Enumerable)
import Enumerate.Enum.Valid (Validatable, isValid)
import MinDepthSN.Data.Size

data Value = Value { channel :: Channel, betweenLayers :: BetweenLayers }
    deriving (Eq, Generic, Enumerable)

instance Show Value where
    show (Value i k) = "Value " ++ show i ++ " " ++ show k

instance Ord Value where
    compare = comparing betweenLayers <> comparing channel

instance Bounded Value where
    minBound = toEnum 0
    maxBound = toEnum (n*(d+1) - 1)

instance Validatable Value where
    isValid = const True

instance Enum Value where
    toEnum int = Value (toEnum i) (toEnum k)
      where
        (k,i) = int `quotRem` n
    fromEnum (Value i k) = fromEnum k * n + fromEnum i

inputValues :: [Value]
inputValues = [ Value i beforeFirstLayer | i <- channels ]

outputValues :: [Value]
outputValues = [ Value i afterLastLayer | i <- channels ]

firstInputValue :: Value
firstInputValue = head inputValues

lastInputValue :: Value
lastInputValue = last inputValues