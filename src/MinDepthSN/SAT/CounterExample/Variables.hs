{-# LANGUAGE DerivingVia #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language MultiParamTypeClasses #-}

module MinDepthSN.SAT.Counterexample.Variables where

import SAT.IPASIR (AsVar(..), Var(..), Dimacs)

import MinDepthSN.Data.Value

newtype Counterexample = Counterexample Value
    deriving newtype (Eq, Ord, Bounded, Enum)
    deriving Dimacs via (Var Counterexample)

instance Show Counterexample where
    show (Counterexample value) = "CEx (" ++ show value ++ ")"

instance AsVar Counterexample Value where
    var = Counterexample

instance AsVar Counterexample Counterexample where
    var = id

