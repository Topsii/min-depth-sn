{-# language PatternSynonyms #-}
{-# language ScopedTypeVariables #-}
{-# language DerivingStrategies #-}

module MinDepthSN.Data.Combinatorics2.CombinationWithRepetition
    ( CombinationWithRepetition(CombinationWithRepetition)
    , zipGEQsWith
    ) where

import Data.Enum (boundedEnumFrom, boundedEnumFromThen)
import Data.Ix
import Data.List (tails)
import Control.Exception (assert)

data CombinationWithRepetition a = MkCombinationWithRepetition a a
    deriving stock (Eq, Ord, Show)

{-# COMPLETE CombinationWithRepetition #-}
pattern CombinationWithRepetition :: Ord a => a -> a -> CombinationWithRepetition a
pattern CombinationWithRepetition x y <- MkCombinationWithRepetition x y
  where
    CombinationWithRepetition x y = assert (x <= y) $ MkCombinationWithRepetition x y

zipGEQsWith :: (a -> a -> b) -> [a] -> [b]
zipGEQsWith f l =
  [ f x y 
  | geqs@(x:_) <- init $ tails l
  , y <- geqs
  ]

instance Ix a => Ix (CombinationWithRepetition a) where
    range = zipGEQsWith CombinationWithRepetition . range . extendBounds
    index b (CombinationWithRepetition x y) = i_x * (size-1) - toTriangular i_x + i_x + i_y
      where
        i_x, i_y, size :: Int
        i_x = index extendedBounds x
        i_y = index extendedBounds y
        size = rangeSize extendedBounds
        extendedBounds :: (a, a)
        extendedBounds = extendBounds b
    inRange b (CombinationWithRepetition x y) =
        inRange extendedBounds x && inRange extendedBounds y
      where
        extendedBounds :: (a, a)
        extendedBounds = extendBounds b

extendBounds :: Ord a => (CombinationWithRepetition a, CombinationWithRepetition a) -> (a, a)
extendBounds (CombinationWithRepetition x1 y1, CombinationWithRepetition x2 y2) =
    (min x1 y1, max x2 y2)

toTriangular :: Int -> Int
toTriangular n = (n * (n + 1)) `div` 2

fromTriangular :: Int -> Int
fromTriangular tri_n = floor $ (sqrt (2 * float_tri_n + 0.25)) - 0.5
  where
    float_tri_n :: Double
    float_tri_n = fromIntegral tri_n

instance (Bounded a, Enum a, Ord a) => Enum (CombinationWithRepetition a) where
    fromEnum (CombinationWithRepetition x1 x2) =
        i_x1 * enumMax - toTriangular i_x1 + i_x1 + i_x2
      where
        i_x1, i_x2, enumMax :: Int
        i_x1 = fromEnum x1
        i_x2 = fromEnum x2
        enumMax = fromEnum (maxBound :: a)
    toEnum n = CombinationWithRepetition (toEnum i_x1) (toEnum i_x2)
      where
        i_x1, i_x2, enumMax, cardA, cardCombNoRep :: Int
        i_x1 = cardA - (1 + fromTriangular (cardCombNoRep - n))
        i_x2 = n - (i_x1 * enumMax - toTriangular i_x1) - i_x1
        enumMax = fromEnum (maxBound :: a)
        cardA = enumMax + 1
        cardCombNoRep = toTriangular (cardA) - 1
    enumFrom = boundedEnumFrom
    enumFromThen = boundedEnumFromThen

instance (Bounded a, Ord a) => Bounded (CombinationWithRepetition a) where
    minBound = CombinationWithRepetition minBound minBound
    maxBound = CombinationWithRepetition maxBound maxBound
