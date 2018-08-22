{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language ScopedTypeVariables #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language DerivingStrategies #-}

{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# language PatternSynonyms #-}

module MinDepthSN.Data.Gate
    ( Gate(Gate, StandardGate, GeneralizedGate)
    , StandardGate
    , GeneralizedGate
    , SortingOrder(..)
    , SortOrder
    , sortOrder
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

data ComparatorGate (o :: SortingOrder) =
    ComparatorGate { minChannel :: Channel, maxChannel :: Channel, layer :: Layer }
    deriving (Eq, Generic, Enumerable, Show)

instance Ord (ComparatorGate o) where
    compare = comparing layer <> comparing minChannel <> comparing maxChannel

instance SortOrder o => Validatable (ComparatorGate o) where
    isValid gate@(ComparatorGate i j _) = case sortOrder gate of
        Standard    | i <  j -> True
        Generalized | i /= j -> True
        _                    -> False

newtype Gate (o :: SortingOrder) = ValidGate (Valid (ComparatorGate o))
        deriving newtype (Eq, Ord, Validatable, Bounded, Enum)

{-# COMPLETE Gate #-}

pattern Gate :: forall o. SortOrder o => Channel -> Channel -> Layer -> Gate o
pattern Gate i j k <- ValidGate (Valid (ComparatorGate i j k)) where
    Gate i j k = case sortOrder gate of
        Standard
            | i < j     -> gate
            | otherwise -> error $ "(Standard) Gate i j k: Requires i < j, "
                ++ "but given was " ++ show i ++ " = i >= j = " ++ show j
        Generalized
            | i /= j    -> gate
            | otherwise -> error $ "(Generalized) Gate i j k: Requires i /= j, "
                ++ "but given was " ++ show i ++ " = i = j = " ++ show j
      where
        gate :: Gate o
        gate = ValidGate (Valid (ComparatorGate i j k))

pattern StandardGate :: Channel -> Channel -> Layer -> StandardGate
pattern StandardGate i j k = Gate i j k

pattern GeneralizedGate :: Channel -> Channel -> Layer -> GeneralizedGate
pattern GeneralizedGate i j k = Gate i j k

instance SortOrder o => Show (Gate o) where
    show gate@(Gate i j k) = show (sortOrder gate) ++ "Gate " 
        ++ show i ++ " " ++ show j ++ " " ++ show k

instance SortOrder o => Enumerable (Gate o) where
    enumerated = boundedEnumerated
    cardinality = boundedCardinality
