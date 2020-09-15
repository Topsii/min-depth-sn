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
    showsPrec p (Counterexample value) = showParen (p >= 11) $
        showString "Cex " . showsPrec 11 value 

instance AsVar Counterexample Value where
    var = Counterexample

instance AsVar Counterexample Counterexample where
    var = id

