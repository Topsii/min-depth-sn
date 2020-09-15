
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language FlexibleContexts #-}
{-# language PatternSynonyms #-}

module MinDepthSN.Data.Value
    ( Value(Value)
    , inputValues
    , outputValues
    , valueLit
    ) where

import Data.Ix
import Generic.Data
import SAT.IPASIR (AsVar(..), Lit, lit)
import MinDepthSN.Data.Size (Channel, BetweenLayers, firstChannel, lastChannel, beforeFirstLayer, afterLastLayer)

-- | @Value i k@ creates a variable \(v_i^k\) representing the value on
-- channel \(i\) between layers \(k\) and \(k+1\).
data Value = MkValue { betweenLayers :: BetweenLayers, channel :: Channel }
    deriving stock (Generic, Eq, Ord, Ix)
    deriving Enum via (FiniteEnumeration Value)
    deriving Bounded via (Generically Value)

{-# COMPLETE Value #-}
pattern Value :: Channel -> BetweenLayers -> Value
pattern Value i k = MkValue k i

instance Show Value where
    showsPrec p (Value i k) = showParen (p >= 11) $
        showString "Value " . showsPrec 11 i . showChar ' ' . showsPrec 11 k 

inputValues :: [Value]
inputValues =
    range
        ( Value firstChannel beforeFirstLayer
        , Value lastChannel  beforeFirstLayer)

outputValues :: [Value]
outputValues =
    range
        ( Value firstChannel afterLastLayer
        , Value lastChannel  afterLastLayer)

-- | Literal of 'Value' with positive polarity.
valueLit :: AsVar v Value => Channel -> BetweenLayers -> Lit v
valueLit i k = lit (Value i k)