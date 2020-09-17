{-# LANGUAGE DerivingVia #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language MultiParamTypeClasses #-}

module MinDepthSN.SAT.CexRun.Variables where

import SAT.IPASIR (AsVar(..), Var(..), Dimacs)

import MinDepthSN.Data.Value

newtype CexRun = CexRun Value
    deriving newtype (Eq, Ord, Bounded, Enum)
    deriving Dimacs via (Var CexRun)

instance Show CexRun where
    showsPrec p (CexRun value) = showParen (p >= 11) $
        showString "CexRun " . showsPrec 11 value 

instance AsVar CexRun Value where
    var = CexRun

instance AsVar CexRun CexRun where
    var = id

