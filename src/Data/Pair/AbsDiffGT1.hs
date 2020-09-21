{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

{-# language PatternSynonyms #-}
{-# language ScopedTypeVariables #-}
{-# language DerivingStrategies #-}

module Data.Pair.AbsDiffGT1 
    ( AbsDiffGT1(AbsDiffGT1)
    ) where

import Data.Ix
import Data.List (tails)
import Data.Enum
import Control.Exception
import Data.Bifunctor (bimap)
import Data.Pair
import Data.Typeable (Typeable)

data AbsDiffGT1 (o :: Order) a = MkAbsDiffGT1 a a
    deriving stock (Eq, Ord, Show)

{-# COMPLETE AbsDiffGT1 #-}
pattern AbsDiffGT1 :: Enum a => a -> a -> AbsDiffGT1 o a
pattern AbsDiffGT1 x y <- MkAbsDiffGT1 x y where
    AbsDiffGT1 x y = assert inv (MkAbsDiffGT1 x y)
      where inv = abs (fromEnum x - fromEnum y) > 1

extendBounds :: (Enum a, Ord a) => (AbsDiffGT1 o a, AbsDiffGT1 o a) -> (a, a)
extendBounds (AbsDiffGT1 x1 y1, AbsDiffGT1 x2 y2) =
    (min x1 y1, max x2 y2)

zipNonImmediateSuccsWith :: (a -> a -> b) -> [a] -> [b]
zipNonImmediateSuccsWith f l =
  [ f x y 
  | (x:_immSucc:nonImmSuccessors) <- init $ tails l
  , y <- nonImmSuccessors
  ]

decrDiff  :: (Typeable o, Enum a, Ord a) => AbsDiffGT1 o a -> Pair o 'NoDuplicates a
decrDiff (AbsDiffGT1 x y)
    | fromEnum x < fromEnum y = Pair x (pred y)
    | otherwise               = Pair (pred x) y

incrDiff :: (Typeable o, Enum a, Ord a) => Pair o 'NoDuplicates a -> AbsDiffGT1 o a
incrDiff s
    | fromEnum x < fromEnum y = AbsDiffGT1 x (succ y)
    | otherwise               = AbsDiffGT1 (succ x) y
  where 
    Pair x y = s

instance (Typeable o, Enum a, Ix a) => Ix (AbsDiffGT1 o a) where
    range = 
      zipNonImmediateSuccsWith AbsDiffGT1 . range . extendBounds
    index b = index (bimap decrDiff decrDiff b) . decrDiff
    inRange b (AbsDiffGT1 x y) = 
        inRange extendedBounds x && inRange extendedBounds y
      where
        extendedBounds :: (a, a)
        extendedBounds = extendBounds b

instance (Typeable o, Bounded a, Enum a, Ord a) =>  Enum (AbsDiffGT1 o a) where
    fromEnum = fromEnum . decrDiff
    toEnum = incrDiff . toEnum
    enumFrom = boundedEnumFrom
    enumFromThen = boundedEnumFromThen

toTriangular :: Int -> Int
toTriangular n = (n * (n + 1)) `div` 2

instance (Typeable o, Bounded a, Enum a, Ord a) => Bounded (AbsDiffGT1 o a) where
    minBound = toEnum 0
    maxBound = toEnum . subtract 1 . toTriangular . subtract 1 $ fromEnum (maxBound :: a)