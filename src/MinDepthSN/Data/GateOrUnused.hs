{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

{-# LANGUAGE DeriveGeneric #-}
{-# language DerivingVia #-}

{-# language FlexibleContexts #-}


module MinDepthSN.Data.GateOrUnused
    ( GateOrUnused
        ( GateOrUnused
        , Gate_
        , Unused_
        )
    , gateOrUnusedLit
    ) where

import Data.Ix
import Generic.Data
import SAT.IPASIR (AsVar(..), Lit, lit)
import MinDepthSN.Data.Size (Channel, Layer)
import MinDepthSN.Data.Gate (Gate(..), KnownNetType, GateChannelOrdering, NetworkType)
import MinDepthSN.Data.Unused (Unused(..))
import MinDepthSN.Data.Combinatorics2.Selection

-- | A variable \(gu_{i,j}^k\) is either a comparator \(g\)ate variable or an 
-- \(u\)nused variable.
--
-- \[
-- gu_{i,j}^k :=    
--      \begin{cases}
--          g_{i,j}^k  & \text{if}\ i \neq j \\
--          unused_i^k & \text{otherwise}
--      \end{cases}
-- \]
--
-- For \(g_{i,j}^k\) see 'MinDepthSN.Data.Gate.Gate' and for \(unused_i^k\) see 'MinDepthSN.Data.Unused.Unused'
--
-- This enables a terser definition of the \(used\), \(once\) and \(update\) constraints
-- from the original encoding of Bundala and Zavodny.
-- See 'MinDepthSN.SAT.Synthesis.Constraints.usageOnce' and 'MinDepthSN.SAT.Synthesis.Constraints.unused'
--
-- The solver does not actually create, consider or solve \(gu_{i,j}^k\) 
-- variables. For any occurrence of \(gu_{i,j}^k\) in formulas or clauses, its 
-- definition is passed to the solver instead.
data GateOrUnused (t :: NetworkType) =
    MkGateOrUnused Layer (Selection (GateChannelOrdering t) 'WithRepetition Channel)
    deriving stock (Generic, Eq, Ord, Ix)
    deriving Enum via (FiniteEnumeration (GateOrUnused t))
    deriving Bounded via (Generically (GateOrUnused t))

instance KnownNetType t => Show (GateOrUnused t) where
    showsPrec p (Gate_ g)   = showParen (p >= 11) $ showString "Gate_ "   . showsPrec 11 g 
    showsPrec p (Unused_ u) = showParen (p >= 11) $ showString "Unused_ " . showsPrec 11 u

{-# COMPLETE Gate_, Unused_ #-}
{-# COMPLETE GateOrUnused   #-}

pattern Gate_ :: KnownNetType t => Gate t -> GateOrUnused t
pattern Gate_ g <- (match -> Left g) where
    Gate_ (Gate i j k) = GateOrUnused i j k

pattern Unused_ :: KnownNetType t => Unused -> GateOrUnused t
pattern Unused_ u <- (match -> Right u) where
    Unused_ (Unused i k) = GateOrUnused i i k

pattern GateOrUnused :: KnownNetType t => Channel -> Channel -> Layer -> GateOrUnused t
pattern GateOrUnused i j k = MkGateOrUnused k (Selection i j)

match :: KnownNetType t => GateOrUnused t -> Either (Gate t) Unused
match (GateOrUnused i j k)
    | i /= j    = Left $ Gate i j k
    | otherwise = Right $ Unused i k

-- | Literal of 'GateOrUnused' with positive polarity.
gateOrUnusedLit :: forall v t. (KnownNetType t, AsVar (v t) (GateOrUnused t)) => Channel -> Channel -> Layer -> Lit (v t)
gateOrUnusedLit i j k = lit (GateOrUnused i j k :: GateOrUnused t)