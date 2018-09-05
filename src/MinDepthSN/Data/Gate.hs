{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language ScopedTypeVariables #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language DerivingStrategies #-}

{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# language PatternSynonyms #-}

{-# language FlexibleContexts #-}

module MinDepthSN.Data.Gate
    ( Gate
        ( Gate
        -- , StandardGate
        -- , GeneralizedGate
        )
    , StandardGate
    , GeneralizedGate
    , SortingOrder(..)
    , SortOrder
    , sortOrder
    , gateLit
    ) where

import Data.Ord (comparing)
import Data.Monoid ((<>))
import GHC.Generics (Generic)
import Enumerate
    ( Enumerable
    , enumerated
    , boundedEnumerated
    , cardinality
    , boundedCardinality
    )
import Enumerate.Enum.Valid (Validatable, Valid(..), isValid)
import SAT.IPASIR (AsVar(..), Lit, lit)
import MinDepthSN.Data.Size (Channel, Layer)

data SortingOrder = Standard | Generalized
    deriving (Eq, Show)

class SortOrder (o :: SortingOrder) where
    sortOrder :: proxy o -> SortingOrder

instance SortOrder 'Standard where
    sortOrder _ = Standard

instance SortOrder 'Generalized where
    sortOrder _ = Generalized

type StandardGate = Gate 'Standard
type GeneralizedGate = Gate 'Generalized

data UnvalidatedGate (o :: SortingOrder) =
    UnvalidatedGate { minChannel :: Channel, maxChannel :: Channel, layer :: Layer }
    deriving (Eq, Generic, Enumerable, Show)

instance Ord (UnvalidatedGate o) where
    compare = comparing layer <> comparing minChannel <> comparing maxChannel

instance SortOrder o => Validatable (UnvalidatedGate o) where
    isValid gate@(UnvalidatedGate i j _) = case sortOrder gate of
        Standard    | i <  j -> True
        Generalized | i /= j -> True
        _                    -> False

-- | @Gate i j k@ creates a variable \(g_{i,j}^k\) representing a
-- comparator gate where \(i\) and \(j\) are the channels and \(k\) is the layer.
newtype Gate (o :: SortingOrder) = ValidGate (Valid (UnvalidatedGate o))
        deriving newtype (Eq, Ord, Validatable, Bounded, Enum)

{-# COMPLETE Gate #-}

pattern Gate :: forall o. SortOrder o => Channel -> Channel -> Layer -> Gate o
pattern Gate i j k = ValidGate (Valid (UnvalidatedGate i j k))

-- DataKinds creates a type identifier from this, that conflicts with the typesynonym StandardGate
-- pattern StandardGate :: Channel -> Channel -> Layer -> StandardGate
-- pattern StandardGate i j k = Gate i j k

-- DataKinds creates a type identifier from this, that conflicts with the typesynonym GeneralizedGate
-- pattern GeneralizedGate :: Channel -> Channel -> Layer -> GeneralizedGate
-- pattern GeneralizedGate i j k = Gate i j k

instance SortOrder o => Show (Gate o) where
    show gate@(Gate i j k) = show (sortOrder gate) ++ "Gate " 
        ++ show i ++ " " ++ show j ++ " " ++ show k

instance SortOrder o => Enumerable (Gate o) where
    enumerated = boundedEnumerated
    cardinality = boundedCardinality

-- | Literal of 'Gate' with positive polarity.
gateLit 
    :: forall v o. (AsVar (v o) (Gate o), SortOrder o) 
    => Channel -> Channel -> Layer -> Lit (v o)
gateLit i j k = lit (Gate i j k :: Gate o)
