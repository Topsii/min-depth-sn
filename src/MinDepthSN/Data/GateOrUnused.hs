{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}


{-# language FlexibleContexts #-}

module MinDepthSN.Data.GateOrUnused
    ( GateOrUnused
        ( GateOrUnused
        -- , GeneralizedGateOrUnused
        -- , StandardGateOrUnused
        , Gate
        , StandardGate
        , GeneralizedGate
        , Unused
        , StandardGate_
        , GeneralizedGate_
        , ..
        )
    , SortingOrder(..)
    , SortOrder
    , StandardGateOrUnused
    , GeneralizedGateOrUnused
    , gateOrUnusedLit
    ) where
import Enumerate
    ( Enumerable
    , enumerated
    , boundedEnumerated
    , cardinality
    , boundedCardinality
    )
import SAT.IPASIR (AsVar(..), Lit, lit)
import MinDepthSN.Data.Size (Channel, Layer)
import MinDepthSN.Data.Gate 
    ( Gate
    , StandardGate
    , GeneralizedGate
    , SortOrder
    , SortingOrder(..)
    , SortOrder
    )
import MinDepthSN.Data.Unused (Unused)
import qualified MinDepthSN.Data.Gate as Gate
import qualified MinDepthSN.Data.Unused as Unused

type StandardGateOrUnused = GateOrUnused 'Standard
type GeneralizedGateOrUnused = GateOrUnused 'Generalized

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
data GateOrUnused (o :: SortingOrder) 
    = Gate_ (Gate o)
    | Unused_ Unused
    deriving (Eq, Ord)

{-# COMPLETE  Gate,  Unused #-}
{-# COMPLETE  Gate, Unused_ #-}
{-# COMPLETE Gate_,  Unused #-}
{-# COMPLETE Gate_, Unused_ #-}
{-# COMPLETE GateOrUnused   #-}

pattern StandardGate_ :: StandardGate -> StandardGateOrUnused
pattern StandardGate_ gate = Gate_ gate

pattern GeneralizedGate_ :: GeneralizedGate -> GeneralizedGateOrUnused
pattern GeneralizedGate_ gate = Gate_ gate

pattern Gate :: SortOrder o => Channel -> Channel -> Layer -> GateOrUnused o
pattern Gate i j k = Gate_ (Gate.Gate i j k)

pattern StandardGate :: Channel -> Channel -> Layer -> StandardGateOrUnused
pattern StandardGate i j k = Gate i j k

pattern GeneralizedGate :: Channel -> Channel -> Layer -> GeneralizedGateOrUnused
pattern GeneralizedGate i j k = Gate i j k

pattern Unused :: Channel -> Layer -> GateOrUnused o
pattern Unused i k = Unused_ (Unused.Unused i k)

pattern GateOrUnused :: SortOrder o => Channel -> Channel -> Layer -> GateOrUnused o
pattern GateOrUnused i j k <- (matchGateOrUnused -> (i, j, k)) where
    GateOrUnused i j k
        | i /= j = Gate i j k
        | otherwise = Unused i k

matchGateOrUnused :: SortOrder o => GateOrUnused o -> (Channel, Channel, Layer)
matchGateOrUnused gu = case gu of
    Gate i j k -> (i, j, k)
    Unused i k -> (i, i, k)

-- DataKinds creates a type identifier from this, that conflicts with the typesynonym StandardGateOrUnused
-- pattern StandardGateOrUnused :: Channel -> Channel -> Layer -> StandardGateOrUnused
-- pattern StandardGateOrUnused i j k = Gate i j k

-- DataKinds creates a type identifier from this, that conflicts with the typesynonym GeneralizedGateOrUnused
-- pattern GeneralizedGateOrUnused :: Channel -> Channel -> Layer -> GeneralizedGateOrUnused
-- pattern GeneralizedGateOrUnused i j k = Gate i j k

instance SortOrder o => Show (GateOrUnused o) where
    show gu = case gu of
        (Gate_ g)   -> show g
        (Unused_ u) -> show u

instance SortOrder o => Bounded (GateOrUnused o) where
    minBound = min (Gate_ minBound) (Unused_ minBound)
    maxBound = max (Gate_ maxBound) (Unused_ maxBound)

-- offset = sum . init . (0 :) . map cardinality $ [Proxy :: Proxy Unused, Proxy, Proxy]

instance SortOrder o => Enum (GateOrUnused o) where
    toEnum i
        | i < 0 = error $ "toEnum (GateOrUnused): negative argument " ++ show i
        | i <= fromEnum (Gate_   maxBound :: GateOrUnused o) 
            = Gate_   $ toEnum (i -   gateOffset)
        | i <= fromEnum (Unused_ maxBound :: GateOrUnused o)
            = Unused_ $ toEnum (i - unusedOffset)
        | otherwise = error $ "toEnum (GateOrUnused):"
            ++ "argument " ++ show i ++ "exceeds maxBound"
      where
        gateOffset :: Int
        gateOffset = 0
        unusedOffset :: Int
        unusedOffset = fromEnum (Gate_ maxBound :: GateOrUnused o) + 1
    fromEnum gu = case gu of
        Gate_   g ->   gateOffset + fromEnum g
        Unused_ u -> unusedOffset + fromEnum u
      where
        gateOffset :: Int
        gateOffset = 0
        unusedOffset :: Int
        unusedOffset = fromEnum (Gate_ maxBound :: GateOrUnused o) + 1

instance SortOrder o => Enumerable (GateOrUnused o) where
    enumerated = boundedEnumerated
    cardinality = boundedCardinality

-- | Literal of 'GateOrUnused' with positive polarity.
gateOrUnusedLit 
    :: forall v o. (AsVar (v o) (GateOrUnused o), SortOrder o) 
    => Channel -> Channel -> Layer -> Lit (v o)
gateOrUnusedLit i j k = lit (GateOrUnused i j k :: GateOrUnused o)