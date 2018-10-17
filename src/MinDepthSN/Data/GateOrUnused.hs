{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# language FlexibleContexts #-}

{-# language UndecidableInstances #-}

module MinDepthSN.Data.GateOrUnused
    ( GateOrUnused
        ( GateOrUnused
        -- , GeneralizedGateOrUnused
        -- , StandardGateOrUnused
        -- , Gate
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
import Generic.Data
import GHC.Generics
import SAT.IPASIR (AsVar(..), Lit, lit)
import MinDepthSN.Data.Size (Channel, Layer)
import MinDepthSN.Data.Gate
    ( Gate
    , StandardGate
    , GeneralizedGate
    , SortOrder
    , SortingOrder(..)
    , SortOrder
    , Two
    )
import MinDepthSN.Data.Unused (Unused)
import qualified MinDepthSN.Data.Gate as Gate
import qualified MinDepthSN.Data.Unused as Unused
import MinDepthSN.Data.Combinatorics2.CombinationNoRepetition
import MinDepthSN.Data.Combinatorics2.VariationNoRepetition

type StandardGateOrUnused = GateOrUnused CombinationNoRepetition
type GeneralizedGateOrUnused = GateOrUnused VariationNoRepetition

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
data GateOrUnused f
    = Gate_ (Gate f)
    | Unused_ Unused
    deriving (Generic)

    -- temp instance
instance Eq (GateOrUnused f) where
    (==) = undefined
instance Ord (GateOrUnused f) where
    compare = undefined
instance Bounded (GateOrUnused f) where
    minBound = undefined
    maxBound = undefined
instance Enum (GateOrUnused f) where
    fromEnum = undefined
    toEnum = undefined
instance Show (GateOrUnused f) where
    show = undefined

-- deriving instance Eq (f Channel) => Eq (GateOrUnused f)
-- deriving instance Ord (f Channel) => Ord (GateOrUnused f)
-- deriving instance Show (f Channel) => Show (GateOrUnused f)
-- instance Show (GateOrUnused f) where
--     show gu = case gu of
--         (Gate_ g)   -> show g
--         (Unused_ u) -> show u
-- instance Bounded (f Channel) => Bounded (GateOrUnused f) where
--     minBound = gminBound
--     maxBound = gmaxBound
-- instance (Bounded (f Channel), Enum (f Channel)) => Enum (GateOrUnused f) where
--     toEnum = gtoFiniteEnum
--     fromEnum = gfromFiniteEnum
--     enumFrom = gfiniteEnumFrom
--     enumFromThen = gfiniteEnumFromThen
--     enumFromTo = gfiniteEnumFromTo
--     enumFromThenTo = gfiniteEnumFromThenTo

{-# COMPLETE  Gate,  Unused #-}
{-# COMPLETE  Gate, Unused_ #-}
{-# COMPLETE Gate_,  Unused #-}
{-# COMPLETE Gate_, Unused_ #-}
{-# COMPLETE GateOrUnused   #-}

pattern StandardGate_ :: StandardGate -> StandardGateOrUnused
pattern StandardGate_ gate = Gate_ gate

pattern GeneralizedGate_ :: GeneralizedGate -> GeneralizedGateOrUnused
pattern GeneralizedGate_ gate = Gate_ gate

pattern Gate :: Two f Channel => Channel -> Channel -> Layer -> GateOrUnused f
pattern Gate i j k = Gate_ (Gate.Gate i j k)

pattern StandardGate :: Channel -> Channel -> Layer -> StandardGateOrUnused
pattern StandardGate i j k = Gate i j k

pattern GeneralizedGate :: Channel -> Channel -> Layer -> GeneralizedGateOrUnused
pattern GeneralizedGate i j k = Gate i j k

pattern Unused :: Channel -> Layer -> GateOrUnused f
pattern Unused i k = Unused_ (Unused.Unused i k)

pattern GateOrUnused :: Two f Channel => Channel -> Channel -> Layer -> GateOrUnused f
pattern GateOrUnused i j k <- (matchGateOrUnused -> (i, j, k)) where
    GateOrUnused i j k
        | i /= j = Gate i j k
        | otherwise = Unused i k

matchGateOrUnused :: Two f Channel => GateOrUnused f -> (Channel, Channel, Layer)
matchGateOrUnused gu = case gu of
    Gate i j k -> (i, j, k)
    Unused i k -> (i, i, k)

-- DataKinds creates a type identifier from this, that conflicts with the typesynonym StandardGateOrUnused
-- pattern StandardGateOrUnused :: Channel -> Channel -> Layer -> StandardGateOrUnused
-- pattern StandardGateOrUnused i j k = Gate i j k

-- DataKinds creates a type identifier from this, that conflicts with the typesynonym GeneralizedGateOrUnused
-- pattern GeneralizedGateOrUnused :: Channel -> Channel -> Layer -> GeneralizedGateOrUnused
-- pattern GeneralizedGateOrUnused i j k = Gate i j k


-- offset = sum . init . (0 :) . map cardinality $ [Proxy :: Proxy Unused, Proxy, Proxy]


instance Enumerable (GateOrUnused f) where
    enumerated = boundedEnumerated
    cardinality = boundedCardinality

-- | Literal of 'GateOrUnused' with positive polarity.
gateOrUnusedLit
    :: forall v f. (AsVar (v f) (GateOrUnused f), Two f Channel) 
    => Channel -> Channel -> Layer -> Lit (v f)
gateOrUnusedLit i j k = lit (GateOrUnused i j k :: GateOrUnused f)