{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}

module Data.Pair
    ( Pair(Pair)
    , Order(..)
    , Duplicates(..)
    ) where

import Data.Pair.UnorderedNoDuplicates ( UnorderedNoDuplicates(..) )
import Data.Pair.OrderedNoDuplicates ( OrderedNoDuplicates(..) )
import Data.Pair.UnorderedWithDuplicates ( UnorderedWithDuplicates(..) )
import Data.Pair.OrderedWithDuplicates ( OrderedWithDuplicates(..) )

import Data.Pair.OrderAndDup
import Data.Typeable ( Typeable )

import Data.Ix
import Data.Enum (boundedEnumFrom, boundedEnumFromThen)


data Pair (o :: Order) (d :: Duplicates) a where
    MkUnorNoDup :: UnorderedNoDuplicates a -> Pair 'Unordered 'NoDuplicates a
    MkOrdeNoDup :: OrderedNoDuplicates a -> Pair 'Ordered 'NoDuplicates a
    MkUnorWiDup :: UnorderedWithDuplicates a -> Pair 'Unordered 'WithDuplicates a
    MkOrdeWiDup :: OrderedWithDuplicates a -> Pair 'Ordered 'WithDuplicates a

{-# COMPLETE Pair #-}
pattern Pair :: forall o d a. (Typeable o, Typeable d, Ord a) => a -> a -> Pair o d a
pattern Pair x y <- (matchPair -> (x, y)) where
    Pair x y = case (orderAndDup :: OrderAndDup o d) of
        UND -> MkUnorNoDup (UnorderedNoDuplicates x y)
        OND -> MkOrdeNoDup (OrderedNoDuplicates x y)
        UWD -> MkUnorWiDup (UnorderedWithDuplicates x y)
        OWD -> MkOrdeWiDup (OrderedWithDuplicates x y)

matchPair :: Ord a => Pair o d a -> (a, a)
matchPair s = case s of
    MkUnorNoDup (UnorderedNoDuplicates x y)   -> (x, y)
    MkOrdeNoDup (OrderedNoDuplicates x y)     -> (x, y)
    MkUnorWiDup (UnorderedWithDuplicates x y) -> (x, y)
    MkOrdeWiDup (OrderedWithDuplicates x y)   -> (x, y)

deriving stock instance Eq a => Eq (Pair o d a)
deriving stock instance Ord a => Ord (Pair o d a)
deriving stock instance Show a => Show (Pair o d a)

instance Ix a => Ix (Pair o d a) where
    range (MkUnorNoDup l, MkUnorNoDup u) = map MkUnorNoDup $ range (l,u)
    range (MkOrdeNoDup l, MkOrdeNoDup u) = map MkOrdeNoDup $ range (l,u)
    range (MkUnorWiDup l, MkUnorWiDup u) = map MkUnorWiDup $ range (l,u)
    range (MkOrdeWiDup l, MkOrdeWiDup u) = map MkOrdeWiDup $ range (l,u)
    index (MkUnorNoDup l, MkUnorNoDup u) (MkUnorNoDup x) = index (l,u) x
    index (MkOrdeNoDup l, MkOrdeNoDup u) (MkOrdeNoDup x) = index (l,u) x
    index (MkUnorWiDup l, MkUnorWiDup u) (MkUnorWiDup x) = index (l,u) x
    index (MkOrdeWiDup l, MkOrdeWiDup u) (MkOrdeWiDup x) = index (l,u) x
    inRange (MkUnorNoDup l, MkUnorNoDup u) (MkUnorNoDup x) = inRange (l,u) x
    inRange (MkOrdeNoDup l, MkOrdeNoDup u) (MkOrdeNoDup x) = inRange (l,u) x
    inRange (MkUnorWiDup l, MkUnorWiDup u) (MkUnorWiDup x) = inRange (l,u) x
    inRange (MkOrdeWiDup l, MkOrdeWiDup u) (MkOrdeWiDup x) = inRange (l,u) x
    rangeSize (MkUnorNoDup l, MkUnorNoDup u) = rangeSize (l,u)
    rangeSize (MkOrdeNoDup l, MkOrdeNoDup u) = rangeSize (l,u)
    rangeSize (MkUnorWiDup l, MkUnorWiDup u) = rangeSize (l,u)
    rangeSize (MkOrdeWiDup l, MkOrdeWiDup u) = rangeSize (l,u)

instance (Typeable o, Typeable d, Bounded a, Enum a, Ord a) => Enum (Pair o d a) where
    fromEnum (MkUnorNoDup x) = fromEnum x
    fromEnum (MkOrdeNoDup x) = fromEnum x
    fromEnum (MkUnorWiDup x) = fromEnum x
    fromEnum (MkOrdeWiDup x) = fromEnum x
    toEnum = case (orderAndDup :: OrderAndDup o d) of
        UND -> MkUnorNoDup . toEnum
        OND -> MkOrdeNoDup . toEnum
        UWD -> MkUnorWiDup . toEnum
        OWD -> MkOrdeWiDup . toEnum
    enumFrom = boundedEnumFrom
    enumFromThen = boundedEnumFromThen

instance (Typeable o, Typeable d, Bounded a, Enum a, Ord a) => Bounded (Pair o d a) where
    minBound = case (orderAndDup :: OrderAndDup o d) of
        UND -> MkUnorNoDup minBound
        OND -> MkOrdeNoDup minBound
        UWD -> MkUnorWiDup minBound
        OWD -> MkOrdeWiDup minBound
    maxBound = case (orderAndDup :: OrderAndDup o d) of
        UND -> MkUnorNoDup maxBound
        OND -> MkOrdeNoDup maxBound
        UWD -> MkUnorWiDup maxBound
        OWD -> MkOrdeWiDup maxBound
