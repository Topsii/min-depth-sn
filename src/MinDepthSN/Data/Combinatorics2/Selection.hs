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

module MinDepthSN.Data.Combinatorics2.Selection where

import MinDepthSN.Data.Combinatorics2.CombinationNoRepetition
    ( CombinationNoRepetition(..) )
import MinDepthSN.Data.Combinatorics2.VariationNoRepetition
    ( VariationNoRepetition(..) )
import MinDepthSN.Data.Combinatorics2.CombinationWithRepetition
    ( CombinationWithRepetition(..) )
import MinDepthSN.Data.Combinatorics2.VariationWithRepetition
    ( VariationWithRepetition(..) )
import Data.Ix
import Data.Typeable (eqT)
import Type.Reflection
import Data.Enum (boundedEnumFrom, boundedEnumFromThen)

data Order = Ordered | Unordered
data Repetition = WithRepetition | NoRepetition

data Selection (o :: Order) (r :: Repetition) a where
    MkCombNoRep :: CombinationNoRepetition a -> Selection 'Unordered 'NoRepetition a
    MkVariNoRep :: VariationNoRepetition a -> Selection 'Ordered 'NoRepetition a
    MkCombWiRep :: CombinationWithRepetition a -> Selection 'Unordered 'WithRepetition a
    MkVariWiRep :: VariationWithRepetition a -> Selection 'Ordered 'WithRepetition a

pattern Selection :: forall o r a. (Typeable o, Typeable r, Ord a) => a -> a -> Selection o r a
pattern Selection x y <- (matchSelection -> (x,y)) where
    Selection x y = case (orderAndRep :: OrderAndRep o r) of
        UNR -> MkCombNoRep (CombinationNoRepetition x y)
        ONR -> MkVariNoRep (VariationNoRepetition x y)
        UWR -> MkCombWiRep (CombinationWithRepetition x y)
        OWR -> MkVariWiRep (VariationWithRepetition x y)

matchSelection :: Ord a => Selection o r a -> (a,a)
matchSelection s = case s of
    MkCombNoRep (CombinationNoRepetition x y)   -> (x, y)
    MkVariNoRep (VariationNoRepetition x y)     -> (x, y)
    MkCombWiRep (CombinationWithRepetition x y) -> (x, y)
    MkVariWiRep (VariationWithRepetition x y)   -> (x, y)

deriving stock instance Eq a => Eq (Selection o r a)
deriving stock instance Ord a => Ord (Selection o r a)
deriving stock instance Show a => Show (Selection o r a)

instance Ix a => Ix (Selection o r a) where
    range (MkCombNoRep l, MkCombNoRep h) = map MkCombNoRep $ range (l,h)
    range (MkVariNoRep l, MkVariNoRep h) = map MkVariNoRep $ range (l,h)
    range (MkCombWiRep l, MkCombWiRep h) = map MkCombWiRep $ range (l,h)
    range (MkVariWiRep l, MkVariWiRep h) = map MkVariWiRep $ range (l,h)
    index (MkCombNoRep l, MkCombNoRep h) (MkCombNoRep x) = index (l,h) x
    index (MkVariNoRep l, MkVariNoRep h) (MkVariNoRep x) = index (l,h) x
    index (MkCombWiRep l, MkCombWiRep h) (MkCombWiRep x) = index (l,h) x
    index (MkVariWiRep l, MkVariWiRep h) (MkVariWiRep x) = index (l,h) x
    inRange (MkCombNoRep l, MkCombNoRep h) (MkCombNoRep x) = inRange (l,h) x
    inRange (MkVariNoRep l, MkVariNoRep h) (MkVariNoRep x) = inRange (l,h) x
    inRange (MkCombWiRep l, MkCombWiRep h) (MkCombWiRep x) = inRange (l,h) x
    inRange (MkVariWiRep l, MkVariWiRep h) (MkVariWiRep x) = inRange (l,h) x

instance (Typeable o, Typeable r, Bounded a, Enum a, Ord a) => Enum (Selection o r a) where
    fromEnum (MkCombNoRep x) = fromEnum x
    fromEnum (MkVariNoRep x) = fromEnum x
    fromEnum (MkCombWiRep x) = fromEnum x
    fromEnum (MkVariWiRep x) = fromEnum x
    toEnum = case (orderAndRep :: OrderAndRep o r) of
        UNR -> MkCombNoRep . toEnum
        ONR -> MkVariNoRep . toEnum
        UWR -> MkCombWiRep . toEnum
        OWR -> MkVariWiRep . toEnum
    enumFrom = boundedEnumFrom
    enumFromThen = boundedEnumFromThen

instance (Typeable o, Typeable r, Bounded a, Enum a, Ord a) => Bounded (Selection o r a) where
    minBound = case (orderAndRep :: OrderAndRep o r) of
        UNR -> MkCombNoRep minBound
        ONR -> MkVariNoRep minBound
        UWR -> MkCombWiRep minBound
        OWR -> MkVariWiRep minBound
    maxBound = case (orderAndRep :: OrderAndRep o r) of
        UNR -> MkCombNoRep maxBound
        ONR -> MkVariNoRep maxBound
        UWR -> MkCombWiRep maxBound
        OWR -> MkVariWiRep maxBound

orderAndRep :: forall o r. (Typeable o, Typeable r) => OrderAndRep o r
orderAndRep = case eqT :: Maybe ('Unordered :~: o) of
    Just Refl -> case eqT :: Maybe ('NoRepetition :~: r) of
        Just Refl -> UNR
        Nothing   -> case eqT:: Maybe ('WithRepetition :~: r) of
            Just Refl -> UWR
            Nothing   -> error "bad"
    Nothing   -> case eqT:: Maybe ('Ordered :~: o) of
        Just Refl -> case eqT :: Maybe ('NoRepetition :~: r) of
            Just Refl -> ONR
            Nothing   -> case eqT:: Maybe ('WithRepetition :~: r) of
                Just Refl -> OWR
                Nothing   -> error "bad"
        Nothing   -> error "bad"

data OrderAndRep (o :: Order) (r :: Repetition) where
    UNR :: OrderAndRep 'Unordered 'NoRepetition
    ONR :: OrderAndRep 'Ordered   'NoRepetition
    UWR :: OrderAndRep 'Unordered 'WithRepetition
    OWR :: OrderAndRep 'Ordered   'WithRepetition

