{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language MultiParamTypeClasses #-}

module MinDepthSN.SAT.CounterExample.Variables where

import Enumerate (Enumerable, enumerated, boundedEnumerated, cardinality, boundedCardinality)
import Enumerate.Enum.Valid (Validatable)

import SAT.IPASIR (AsVar(..), Var(..), Lit(..))

import MinDepthSN.Data.Size
import MinDepthSN.Data.Value

newtype CounterExample = CounterExample Value
    deriving (Eq, Ord, Bounded, Validatable, Enum)

pattern ValueVar :: Channel -> BetweenLayers -> Var CounterExample
pattern ValueVar i k = Var (CounterExample (Value i k))

instance Show CounterExample where
    show (CounterExample value) = "CEx (" ++ show value ++ ")"

instance Enumerable CounterExample where
    enumerated = boundedEnumerated
    cardinality = boundedCardinality

instance AsVar CounterExample Value where
    v = CounterExample

valueLit :: Channel -> BetweenLayers -> Lit CounterExample
valueLit i k = Positive (ValueVar i k)