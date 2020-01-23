
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
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
    deriving (Generic, Eq, Ord)
    deriving Enum via (FiniteEnumeration Value)
    deriving Bounded via (Generically Value)

{-# COMPLETE Value #-}
pattern Value :: Channel -> BetweenLayers -> Value
pattern Value i k = MkValue k i

instance Show Value where
    show (Value i k) = "Value " ++ show i ++ " " ++ show k

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
