{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}

module MinDepthSN.SAT.CounterExample.Variables where

import Enumerate (Enumerable, enumerated, boundedEnumerated, cardinality, boundedCardinality)
import Enumerate.Enum.Valid (Validatable)

import SAT.IPASIR.EnumVars (Var(..), Lit(..))

import MinDepthSN.Data.Size
import MinDepthSN.Data.Value

newtype CounterExample = CounterExample Value
    deriving (Eq, Ord, Bounded, Validatable, Enum)

pattern ValueVar :: Channel -> BetweenLayers -> Var CounterExample
pattern ValueVar i k = Var (CounterExample (Value i k))

instance Show CounterExample where
    show (CounterExample v) = "CEx (" ++ show v ++ ")"

instance Enumerable CounterExample where
    enumerated = boundedEnumerated
    cardinality = boundedCardinality

valueLit :: Channel -> BetweenLayers -> Lit CounterExample
valueLit i k = Pos (ValueVar i k)

firstInputValueVar :: Var CounterExample
firstInputValueVar = Var . CounterExample $ firstInputValue

lastInputValueVar :: Var CounterExample
lastInputValueVar = Var . CounterExample $ lastInputValue

minCounterExample :: Var CounterExample
minCounterExample = Var minBound

maxCounterExample :: Var CounterExample
maxCounterExample = Var maxBound