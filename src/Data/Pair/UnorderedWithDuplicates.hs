{-# language PatternSynonyms #-}
{-# language ScopedTypeVariables #-}
{-# language DerivingStrategies #-}

module Data.Pair.UnorderedWithDuplicates
    ( UnorderedWithDuplicates(UnorderedWithDuplicates)
    , zipGEQsWith
    ) where

import Data.Enum (boundedEnumFrom, boundedEnumFromThen)
import Data.Ix
import Data.List (tails)
import Control.Exception (assert)

data UnorderedWithDuplicates a = MkUnorderedWithDuplicates a a
    deriving stock (Eq, Ord, Show)

{-# COMPLETE UnorderedWithDuplicates #-}
pattern UnorderedWithDuplicates :: Ord a => a -> a -> UnorderedWithDuplicates a
pattern UnorderedWithDuplicates x y <- MkUnorderedWithDuplicates x y
  where
    UnorderedWithDuplicates x y = assert (x <= y) $ MkUnorderedWithDuplicates x y

zipGEQsWith :: (a -> a -> b) -> [a] -> [b]
zipGEQsWith f l =
  [ f x y 
  | geqs@(x:_) <- init $ tails l
  , y <- geqs
  ]

instance Ix a => Ix (UnorderedWithDuplicates a) where
    range = zipGEQsWith UnorderedWithDuplicates . range . extendBounds
    index b (UnorderedWithDuplicates x y) = i_x * (size-1) - toTriangular i_x + i_x + i_y
      where
        i_x, i_y, size :: Int
        i_x = index extendedBounds x
        i_y = index extendedBounds y
        size = rangeSize extendedBounds
        extendedBounds :: (a, a)
        extendedBounds = extendBounds b
    inRange b (UnorderedWithDuplicates x y) =
        inRange extendedBounds x && inRange extendedBounds y
      where
        extendedBounds :: (a, a)
        extendedBounds = extendBounds b
    rangeSize b = aSize + (aSize * (aSize - 1)) `div` 2 -- use triangularSize
      where
        aSize = rangeSize $ extendBounds b

extendBounds :: Ord a => (UnorderedWithDuplicates a, UnorderedWithDuplicates a) -> (a, a)
extendBounds (UnorderedWithDuplicates l1 l2, UnorderedWithDuplicates u1 u2) =
    (min l1 l2, max u1 u2)

toTriangular :: Int -> Int
toTriangular n = (n * (n + 1)) `div` 2

fromTriangular :: Int -> Int
fromTriangular tri_n = floor $ (sqrt (2 * float_tri_n + 0.25)) - 0.5
  where
    float_tri_n :: Double
    float_tri_n = fromIntegral tri_n

instance (Bounded a, Enum a, Ord a) => Enum (UnorderedWithDuplicates a) where
    fromEnum (UnorderedWithDuplicates x1 x2) =
        i_x1 * enumMax - toTriangular i_x1 + i_x1 + i_x2
      where
        i_x1, i_x2, enumMax :: Int
        i_x1 = fromEnum x1
        i_x2 = fromEnum x2
        enumMax = fromEnum (maxBound :: a)
    toEnum n = UnorderedWithDuplicates (toEnum i_x1) (toEnum i_x2)
      where
        i_x1, i_x2, enumMax, cardA, cardCombNoRep :: Int
        i_x1 = cardA - (1 + fromTriangular (cardCombNoRep - n))
        i_x2 = n - (i_x1 * enumMax - toTriangular i_x1) - i_x1
        enumMax = fromEnum (maxBound :: a)
        cardA = enumMax + 1
        cardCombNoRep = toTriangular (cardA) - 1
    enumFrom = boundedEnumFrom
    enumFromThen = boundedEnumFromThen

instance (Bounded a, Ord a) => Bounded (UnorderedWithDuplicates a) where
    minBound = UnorderedWithDuplicates minBound minBound
    maxBound = UnorderedWithDuplicates maxBound maxBound
