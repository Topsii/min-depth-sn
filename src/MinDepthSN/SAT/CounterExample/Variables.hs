{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language MultiParamTypeClasses #-}

module MinDepthSN.SAT.CounterExample.Variables where

import SAT.IPASIR (AsVar(..))

import MinDepthSN.Data.Value

newtype CounterExample = CounterExample Value
    deriving (Eq, Ord, Bounded, Enum)

instance Show CounterExample where
    show (CounterExample value) = "CEx (" ++ show value ++ ")"

instance AsVar CounterExample Value where
    asVar = CounterExample

instance AsVar CounterExample CounterExample where
    asVar = id

