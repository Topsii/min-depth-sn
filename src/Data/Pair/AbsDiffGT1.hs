{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

{-# language PatternSynonyms #-}
{-# language ScopedTypeVariables #-}
{-# language DerivingVia #-}
{-# language DeriveGeneric #-}

module Data.Pair.AbsDiffGT1 
    ( AbsDiffGT1(AbsDiffGT1)
    ) where

import Generic.Data
import Data.Ix
import Data.Enum
import Control.Exception
import Data.Bifunctor (bimap)
import Data.Pair
import Data.Typeable (Typeable)
-- import Data.Finite
-- import MinDepthSN.Data.Size

data AbsDiffGT1 (o :: Order) a = MkAbsDiffGT1 a a
    deriving stock (Eq, Ord, Show)

 -- use smart constructor instead to rule out (MkEnum maxBound)?
newtype DecrEnumBy1 a = MkDecrEnumBy1 a
    deriving stock (Generic, Eq, Ord, Ix)
    deriving Enum via (FiniteEnumeration (DecrEnumBy1 a))

instance (Bounded a, Enum a) => Bounded (DecrEnumBy1 a) where
    minBound = MkDecrEnumBy1 minBound
    maxBound = MkDecrEnumBy1 $ pred maxBound

{-# COMPLETE AbsDiffGT1 #-}
pattern AbsDiffGT1 :: Enum a => a -> a -> AbsDiffGT1 o a
pattern AbsDiffGT1 x y <- MkAbsDiffGT1 x y where
    AbsDiffGT1 x y = assert inv (MkAbsDiffGT1 x y)
      where inv = abs (fromEnum x - fromEnum y) > 1

decrDiff :: (Typeable o, Enum a, Ord a) => AbsDiffGT1 o a -> Pair o 'NoDuplicates (DecrEnumBy1 a)
decrDiff (AbsDiffGT1 x y)
    | fromEnum x < fromEnum y = Pair (MkDecrEnumBy1 x) (MkDecrEnumBy1 $ pred y)
    | otherwise               = Pair (MkDecrEnumBy1 $ pred x) (MkDecrEnumBy1 y)

incrDiff :: (Typeable o, Enum a, Ord a) => Pair o 'NoDuplicates (DecrEnumBy1 a) -> AbsDiffGT1 o a
incrDiff s
    | fromEnum x < fromEnum y = AbsDiffGT1 x (succ y)
    | otherwise               = AbsDiffGT1 (succ x) y
  where 
    Pair (MkDecrEnumBy1 x) (MkDecrEnumBy1 y) = s

instance (Typeable o, Enum a, Ix a) => Ix (AbsDiffGT1 o a) where
    range = map incrDiff . range . bimap decrDiff decrDiff
    index b = index (bimap decrDiff decrDiff b) . decrDiff
    inRange b = inRange (bimap decrDiff decrDiff b) . decrDiff
    rangeSize = rangeSize . bimap decrDiff decrDiff

instance (Typeable o, Bounded a, Enum a, Ord a) =>  Enum (AbsDiffGT1 o a) where
    fromEnum = fromEnum . decrDiff
    toEnum = incrDiff . toEnum
    enumFrom = boundedEnumFrom
    enumFromThen = boundedEnumFromThen

instance (Typeable o, Bounded a, Enum a, Ord a) => Bounded (AbsDiffGT1 o a) where
    minBound = incrDiff minBound
    maxBound = incrDiff maxBound