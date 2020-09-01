{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

{-# LANGUAGE DeriveGeneric #-}
{-# language DerivingVia #-}

{-# language FlexibleContexts #-}

{-# language FlexibleInstances #-}
{-# language StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module MinDepthSN.Data.GateOrUnused
    ( Either(GateOrUnused)
    , gateOrUnusedLit
    ) where
import Generic.Data
import SAT.IPASIR (AsVar(..), Lit, lit)
import MinDepthSN.Data.Size (Channel, Layer)
import MinDepthSN.Data.Gate ( Gate(..))
import MinDepthSN.Data.Unused (Unused(..))

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
{-# COMPLETE GateOrUnused   #-}
pattern GateOrUnused :: Channel -> Channel -> Layer -> Either Gate Unused
pattern GateOrUnused i j k <- (matchGateOrUnused -> (i, j, k)) where
    GateOrUnused i j k
        | i /= j = Left $ Gate i j k
        | otherwise = Right $ Unused i k

matchGateOrUnused :: Either Gate Unused -> (Channel, Channel, Layer)
matchGateOrUnused gu = case gu of
    Left (Gate i j k) -> (i, j, k)
    Right (Unused i k) -> (i, i, k)

deriving via (FiniteEnumeration (Either Gate Unused)) instance Enum (Either Gate Unused)
deriving via (Generically (Either Gate Unused)) instance Bounded (Either Gate Unused)

-- | Literal of 'GateOrUnused' with positive polarity.
gateOrUnusedLit :: forall v. AsVar v (Either Gate Unused) => Channel -> Channel -> Layer -> Lit v
gateOrUnusedLit i j k 
    | i /= j = (lit :: Either Gate Unused -> Lit v) . Left $ Gate i j k
    | otherwise = (lit :: Either Gate Unused -> Lit v) . Right $ Unused i k
