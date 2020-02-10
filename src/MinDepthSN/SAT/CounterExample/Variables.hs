{-# language GeneralizedNewtypeDeriving #-}
{-# language DerivingStrategies #-}
{-# language PatternSynonyms #-}
{-# language MultiParamTypeClasses #-}

module MinDepthSN.SAT.Counterexample.Variables where

import SAT.IPASIR (AsVar(..))

import MinDepthSN.Data.Value

newtype Counterexample = Counterexample Value
    deriving newtype (Eq, Ord, Bounded, Enum)

instance Show Counterexample where
    show (Counterexample value) = "CEx (" ++ show value ++ ")"

instance AsVar Counterexample Value where
    asVar = Counterexample

instance AsVar Counterexample Counterexample where
    asVar = id

