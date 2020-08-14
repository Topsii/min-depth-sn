{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

{-# LANGUAGE DeriveGeneric #-}
{-# language DerivingVia #-}

{-# language FlexibleContexts #-}

{-# language UndecidableInstances #-}

module MinDepthSN.Data.GateOrUnused
    ( GateOrUnused
        ( GateOrUnused
        -- , Gate
        , Unused
        , ..
        )
    , SortingOrder(..)
    , SortOrder
    , gateOrUnusedLit
    ) where
import Generic.Data
import SAT.IPASIR (AsVar(..), Lit, lit)
import MinDepthSN.Data.Size (Channel, Layer)
import MinDepthSN.Data.Gate
    ( Gate
    , SortOrder
    , SortingOrder(..)
    , SortOrder
    )
import MinDepthSN.Data.Unused (Unused)
import qualified MinDepthSN.Data.Gate as Gate
import qualified MinDepthSN.Data.Unused as Unused

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
data GateOrUnused
    = Gate_ Gate
    | Unused_ Unused
    deriving stock (Generic, Eq, Ord, Show)
    deriving Enum via (FiniteEnumeration GateOrUnused)
    deriving Bounded via (Generically GateOrUnused)

{-# COMPLETE  Gate,  Unused #-}
{-# COMPLETE  Gate, Unused_ #-}
{-# COMPLETE Gate_,  Unused #-}
{-# COMPLETE Gate_, Unused_ #-}
{-# COMPLETE GateOrUnused   #-}
pattern Gate :: Channel -> Channel -> Layer -> GateOrUnused
pattern Gate i j k = Gate_ (Gate.Gate i j k)

pattern Unused :: Channel -> Layer -> GateOrUnused
pattern Unused i k = Unused_ (Unused.Unused i k)

pattern GateOrUnused :: Channel -> Channel -> Layer -> GateOrUnused
pattern GateOrUnused i j k <- (matchGateOrUnused -> (i, j, k)) where
    GateOrUnused i j k
        | i /= j = Gate i j k
        | otherwise = Unused i k

matchGateOrUnused :: GateOrUnused -> (Channel, Channel, Layer)
matchGateOrUnused gu = case gu of
    Gate i j k -> (i, j, k)
    Unused i k -> (i, i, k)

-- | Literal of 'GateOrUnused' with positive polarity.
gateOrUnusedLit :: AsVar v GateOrUnused => Channel -> Channel -> Layer -> Lit v
gateOrUnusedLit i j k = lit (GateOrUnused i j k)