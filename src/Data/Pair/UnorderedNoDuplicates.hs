{-# language PatternSynonyms #-}
{-# language ScopedTypeVariables #-}
{-# language DerivingStrategies #-}

module Data.Pair.UnorderedNoDuplicates
    ( UnorderedNoDuplicates(UnorderedNoDuplicates)
    , zipWithSuccs
    ) where

import Data.Enum (boundedEnumFrom, boundedEnumFromThen)
import Data.Ix
import Data.List (tails)
import Control.Exception (assert)

data UnorderedNoDuplicates a = MkUnorderedNoDuplicates a a
    deriving stock (Eq, Ord, Show)

{-# COMPLETE UnorderedNoDuplicates #-}
pattern UnorderedNoDuplicates :: Ord a => a -> a -> UnorderedNoDuplicates a
pattern UnorderedNoDuplicates x y <- MkUnorderedNoDuplicates x y
  where
    UnorderedNoDuplicates x y = assert (x < y) $ MkUnorderedNoDuplicates x y

zipWithSuccs :: (a -> a -> b) -> [a] -> [b]
zipWithSuccs f l =
  [ f x y 
  | (x:successors) <- init $ tails l
  , y <- successors
  ]

instance Ix a => Ix (UnorderedNoDuplicates a) where
    range =
      zipWithSuccs UnorderedNoDuplicates . range . extendBounds
    index b (UnorderedNoDuplicates x y) = i_x * (size-1) - toTriangular i_x - 1 + i_y
      where
        i_x, i_y, size :: Int
        i_x = index extendedBounds x
        i_y = index extendedBounds y
        size = rangeSize extendedBounds
        extendedBounds :: (a, a)
        extendedBounds = extendBounds b
    inRange b (UnorderedNoDuplicates x y) =
        inRange extendedBounds x && inRange extendedBounds y
      where
        extendedBounds :: (a, a)
        extendedBounds = extendBounds b
    rangeSize b = (aSize * (aSize - 1)) `div` 2 -- use triangularSize
      where
        aSize = rangeSize $ extendBounds b

extendBounds :: Ord a => (UnorderedNoDuplicates a, UnorderedNoDuplicates a) -> (a, a)
extendBounds (UnorderedNoDuplicates x1 y1, UnorderedNoDuplicates x2 y2) =
    (min x1 y1, max x2 y2)

toTriangular :: Int -> Int
toTriangular n = (n * (n + 1)) `div` 2

fromTriangular :: Int -> Int
fromTriangular tri_n = floor $ (sqrt (2 * float_tri_n + 0.25)) - 0.5
  where
    float_tri_n :: Double
    float_tri_n = fromIntegral tri_n

instance (Bounded a, Enum a, Ord a) => Enum (UnorderedNoDuplicates a) where
    fromEnum (UnorderedNoDuplicates x1 x2) =
        i_x1 * enumMax - toTriangular i_x1 - 1 + i_x2
      where
        i_x1, i_x2, enumMax :: Int
        i_x1 = fromEnum x1
        i_x2 = fromEnum x2
        enumMax = fromEnum (maxBound :: a)
    toEnum n = UnorderedNoDuplicates (toEnum i_x1) (toEnum i_x2)
      where
        i_x1, i_x2, enumMax, cardA, cardCombNoRep :: Int
        i_x1 = cardA - (2 + fromTriangular (cardCombNoRep - n))
        i_x2 = n - (i_x1 * enumMax - toTriangular i_x1 - 1)
        enumMax = fromEnum (maxBound :: a)
        cardA = enumMax + 1
        cardCombNoRep = toTriangular (cardA - 1) - 1
    enumFrom = boundedEnumFrom
    enumFromThen = boundedEnumFromThen

instance (Bounded a, Enum a, Ord a) => Bounded (UnorderedNoDuplicates a) where
    minBound = toEnum 0
    maxBound = toEnum . subtract 1 . toTriangular $ fromEnum (maxBound :: a)
    