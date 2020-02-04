{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MinDepthSN.Data.Combinatorics2.VariationNoRepetition
    ( VariationNoRepetition(VariationNoRepetition)
    ) where

import Data.Enum (boundedEnumFrom, boundedEnumFromThen)
import Data.Ix
import Control.Exception (assert)

data VariationNoRepetition a = MkVariationNoRepetition a a
    deriving (Eq, Ord, Show)

{-# COMPLETE VariationNoRepetition #-}
pattern VariationNoRepetition :: Eq a => a -> a -> VariationNoRepetition a
pattern VariationNoRepetition a a' <- MkVariationNoRepetition a a'
  where
    VariationNoRepetition a a' = assert (a /= a') $ MkVariationNoRepetition a a'

instance Ix a => Ix (VariationNoRepetition a) where
    range b =
        [
            VariationNoRepetition x y
        | y <- extendedRange
        , x <- extendedRange
        , x /= y
        ]
      where
        extendedRange :: [a]
        extendedRange = range $ extendBounds b
    index b (VariationNoRepetition x y) = i_x * (size-1) + i_y - fromEnum (i_x <= i_y)
      where
        i_x, i_y, size :: Int
        i_x = index extendedBounds x
        i_y = index extendedBounds y
        size = rangeSize extendedBounds
        extendedBounds :: (a, a)
        extendedBounds = extendBounds b
    inRange b (VariationNoRepetition x y) =
        inRange extendedBounds  x && inRange extendedBounds y
      where
        extendedBounds :: (a, a)
        extendedBounds = extendBounds b

extendBounds :: Ord a => (VariationNoRepetition a, VariationNoRepetition a) -> (a, a)
extendBounds (VariationNoRepetition x1 y1, VariationNoRepetition x2 y2) =
    (min x1 y1, max x2 y2)
instance (Bounded a, Enum a, Eq a) => Enum (VariationNoRepetition a) where
    fromEnum (VariationNoRepetition x1 x2) =
        i_x1 * enumMax + i_x2 - fromEnum (i_x1 <= i_x2)
      where
        i_x1, i_x2, enumMax :: Int
        i_x1 = fromEnum x1
        i_x2 = fromEnum x2
        enumMax = fromEnum (maxBound :: a)
    toEnum n = VariationNoRepetition (toEnum i_x1) (toEnum i_x2)
      where
        i_x1, i_x2, enumMax :: Int
        i_x1 = n `div` enumMax
        i_x2' = n - i_x1 * enumMax
        i_x2 = i_x2' + fromEnum (i_x1 <= i_x2')
        enumMax = fromEnum (maxBound :: a)
    enumFrom = boundedEnumFrom
    enumFromThen = boundedEnumFromThen

instance (Bounded a, Enum a, Eq a) => Bounded (VariationNoRepetition a) where
    minBound = toEnum 0
    maxBound = toEnum $ (enumMax + 1) * enumMax - 1
      where
        enumMax = fromEnum (maxBound :: a)