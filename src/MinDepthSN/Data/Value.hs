
{-# language DeriveGeneric #-}
{-# language PatternSynonyms #-}

module MinDepthSN.Data.Value
    ( Value(Value)
    , inputValues
    , outputValues
    ) where

import Generic.Data
import GHC.Generics (Generic)
import MinDepthSN.Data.Size (Channel, BetweenLayers, firstChannel, secondChannel, lastChannel, beforeFirstLayer, afterLastLayer)

-- | @Value i k@ creates a variable \(v_i^k\) representing the value on
-- channel \(i\) between layers \(k\) and \(k+1\).
data Value = MkValue { betweenLayers :: BetweenLayers, channel :: Channel }
    deriving (Eq, Generic, Ord)

{-# COMPLETE Value #-}
pattern Value :: Channel -> BetweenLayers -> Value
pattern Value i k = MkValue k i

instance Show Value where
    show (Value i k) = "Value " ++ show i ++ " " ++ show k

instance Bounded Value where
    minBound = gminBound
    maxBound = gmaxBound

instance Enum Value where
    toEnum = gtoFiniteEnum
    fromEnum = gfromFiniteEnum
    enumFrom = gfiniteEnumFrom
    enumFromThen = gfiniteEnumFromThen
    enumFromTo = gfiniteEnumFromTo
    enumFromThenTo = gfiniteEnumFromThenTo

inputValues :: [Value]
inputValues =
    [ Value  firstChannel beforeFirstLayer
    , Value secondChannel beforeFirstLayer
      .. 
      Value   lastChannel beforeFirstLayer
    ]

outputValues :: [Value]
outputValues =
    [ Value  firstChannel afterLastLayer
    , Value secondChannel afterLastLayer
      .. 
      Value   lastChannel afterLastLayer
    ]
