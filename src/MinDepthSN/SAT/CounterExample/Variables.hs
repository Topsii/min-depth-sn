{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language MultiParamTypeClasses #-}

module MinDepthSN.SAT.CounterExample.Variables where

import SAT.IPASIR (AsVar(..), Var(..), Lit(..))

import MinDepthSN.Data.Size
import MinDepthSN.Data.Value

newtype CounterExample = CounterExample Value
    deriving (Eq, Ord, Bounded, Enum)

pattern ValueVar :: Channel -> BetweenLayers -> Var CounterExample
pattern ValueVar i k = Var (CounterExample (Value i k))

instance Show CounterExample where
    show (CounterExample value) = "CEx (" ++ show value ++ ")"

instance AsVar CounterExample Value where
    asVar = CounterExample

instance AsVar CounterExample CounterExample where
    asVar = id

valueLit :: Channel -> BetweenLayers -> Lit CounterExample
valueLit i k = Positive (ValueVar i k)