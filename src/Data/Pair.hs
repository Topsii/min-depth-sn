{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Data.Pair
    ( Pair(Pair, ..)
    , Order(..)
    , Duplicates(..)
    ) where

import Data.Pair.UnorderedNoDuplicates ( UnorderedNoDuplicates(..) )
import Data.Pair.OrderedNoDuplicates ( OrderedNoDuplicates(..) )
import Data.Pair.UnorderedWithDuplicates ( UnorderedWithDuplicates(..) )
import Data.Pair.OrderedWithDuplicates ( OrderedWithDuplicates(..) )

import Data.Ix
import Data.Typeable (eqT)
import Type.Reflection
import Data.Enum (boundedEnumFrom, boundedEnumFromThen)

data Order = Ordered | Unordered
data Duplicates = WithDuplicates | NoDuplicates

data Pair (o :: Order) (d :: Duplicates) a where
    MkUnorNoDup :: UnorderedNoDuplicates a -> Pair 'Unordered 'NoDuplicates a
    MkOrdeNoDup :: OrderedNoDuplicates a -> Pair 'Ordered 'NoDuplicates a
    MkUnorWiDup :: UnorderedWithDuplicates a -> Pair 'Unordered 'WithDuplicates a
    MkOrdeWiDup :: OrderedWithDuplicates a -> Pair 'Ordered 'WithDuplicates a

pattern Pair :: forall o d a. (Typeable o, Typeable d, Ord a) => a -> a -> Pair o d a
pattern Pair x y <- (matchPair -> (x,y)) where
    Pair x y = case (orderAndDup :: OrderAndDup o d) of
        UND -> MkUnorNoDup (UnorderedNoDuplicates x y)
        OND -> MkOrdeNoDup (OrderedNoDuplicates x y)
        UWD -> MkUnorWiDup (UnorderedWithDuplicates x y)
        OWD -> MkOrdeWiDup (OrderedWithDuplicates x y)

matchPair :: Ord a => Pair o d a -> (a,a)
matchPair s = case s of
    MkUnorNoDup (UnorderedNoDuplicates x y)   -> (x, y)
    MkOrdeNoDup (OrderedNoDuplicates x y)     -> (x, y)
    MkUnorWiDup (UnorderedWithDuplicates x y) -> (x, y)
    MkOrdeWiDup (OrderedWithDuplicates x y)   -> (x, y)

deriving stock instance Eq a => Eq (Pair o d a)
deriving stock instance Ord a => Ord (Pair o d a)
deriving stock instance Show a => Show (Pair o d a)

instance Ix a => Ix (Pair o d a) where
    range (MkUnorNoDup l, MkUnorNoDup h) = map MkUnorNoDup $ range (l,h)
    range (MkOrdeNoDup l, MkOrdeNoDup h) = map MkOrdeNoDup $ range (l,h)
    range (MkUnorWiDup l, MkUnorWiDup h) = map MkUnorWiDup $ range (l,h)
    range (MkOrdeWiDup l, MkOrdeWiDup h) = map MkOrdeWiDup $ range (l,h)
    index (MkUnorNoDup l, MkUnorNoDup h) (MkUnorNoDup x) = index (l,h) x
    index (MkOrdeNoDup l, MkOrdeNoDup h) (MkOrdeNoDup x) = index (l,h) x
    index (MkUnorWiDup l, MkUnorWiDup h) (MkUnorWiDup x) = index (l,h) x
    index (MkOrdeWiDup l, MkOrdeWiDup h) (MkOrdeWiDup x) = index (l,h) x
    inRange (MkUnorNoDup l, MkUnorNoDup h) (MkUnorNoDup x) = inRange (l,h) x
    inRange (MkOrdeNoDup l, MkOrdeNoDup h) (MkOrdeNoDup x) = inRange (l,h) x
    inRange (MkUnorWiDup l, MkUnorWiDup h) (MkUnorWiDup x) = inRange (l,h) x
    inRange (MkOrdeWiDup l, MkOrdeWiDup h) (MkOrdeWiDup x) = inRange (l,h) x

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

orderAndDup :: forall o d. (Typeable o, Typeable d) => OrderAndDup o d
orderAndDup = case eqT :: Maybe ('Unordered :~: o) of
    Just Refl -> case eqT :: Maybe ('NoDuplicates :~: d) of
        Just Refl -> UND
        Nothing   -> case eqT:: Maybe ('WithDuplicates :~: d) of
            Just Refl -> UWD
            Nothing   -> error "bad"
    Nothing   -> case eqT:: Maybe ('Ordered :~: o) of
        Just Refl -> case eqT :: Maybe ('NoDuplicates :~: d) of
            Just Refl -> OND
            Nothing   -> case eqT:: Maybe ('WithDuplicates :~: d) of
                Just Refl -> OWD
                Nothing   -> error "bad"
        Nothing   -> error "bad"

data OrderAndDup (o :: Order) (d :: Duplicates) where
    UND :: OrderAndDup 'Unordered 'NoDuplicates
    OND :: OrderAndDup 'Ordered   'NoDuplicates
    UWD :: OrderAndDup 'Unordered 'WithDuplicates
    OWD :: OrderAndDup 'Ordered   'WithDuplicates

