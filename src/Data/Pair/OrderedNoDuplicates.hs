{-# language PatternSynonyms #-}
{-# language ScopedTypeVariables #-}
{-# language DerivingStrategies #-}

module Data.Pair.OrderedNoDuplicates
    ( OrderedNoDuplicates(OrderedNoDuplicates)
    ) where

import GHC.Enum (boundedEnumFrom, boundedEnumFromThen)
import Data.Ix
import Control.Exception (assert)

data OrderedNoDuplicates a = MkOrderedNoDuplicates a a
    deriving stock (Eq, Ord, Show)

{-# COMPLETE OrderedNoDuplicates #-}
pattern OrderedNoDuplicates :: Eq a => a -> a -> OrderedNoDuplicates a
pattern OrderedNoDuplicates a a' <- MkOrderedNoDuplicates a a'
  where
    OrderedNoDuplicates a a' = assert (a /= a') $ MkOrderedNoDuplicates a a'

instance Ix a => Ix (OrderedNoDuplicates a) where
    range b =
        [ OrderedNoDuplicates x y
        | x <- extendedRange
        , y <- extendedRange
        , x /= y
        ]
      where
        extendedRange :: [a]
        extendedRange = range $ extendBounds b
    index b (OrderedNoDuplicates x y) = i_x * (size-1) + i_y - fromEnum (i_x <= i_y)
      where
        i_x, i_y, size :: Int
        i_x = index extendedBounds x
        i_y = index extendedBounds y
        size = rangeSize extendedBounds
        extendedBounds :: (a, a)
        extendedBounds = extendBounds b
    inRange b (OrderedNoDuplicates x y) =
        inRange extendedBounds x && inRange extendedBounds y
      where
        extendedBounds :: (a, a)
        extendedBounds = extendBounds b

extendBounds :: Ord a => (OrderedNoDuplicates a, OrderedNoDuplicates a) -> (a, a)
extendBounds (OrderedNoDuplicates x1 y1, OrderedNoDuplicates x2 y2) =
    (min x1 y1, max x2 y2)
instance (Bounded a, Enum a, Eq a) => Enum (OrderedNoDuplicates a) where
    fromEnum (OrderedNoDuplicates x1 x2) =
        i_x1 * enumMax + i_x2 - fromEnum (i_x1 <= i_x2)
      where
        i_x1, i_x2, enumMax :: Int
        i_x1 = fromEnum x1
        i_x2 = fromEnum x2
        enumMax = fromEnum (maxBound :: a)
    toEnum n = OrderedNoDuplicates (toEnum i_x1) (toEnum i_x2)
      where
        i_x1, i_x2, enumMax :: Int
        i_x1 = n `div` enumMax
        i_x2' = n - i_x1 * enumMax
        i_x2 = i_x2' + fromEnum (i_x1 <= i_x2')
        enumMax = fromEnum (maxBound :: a)
    enumFrom = boundedEnumFrom
    enumFromThen = boundedEnumFromThen

instance (Bounded a, Enum a, Eq a) => Bounded (OrderedNoDuplicates a) where
    minBound = toEnum 0
    maxBound = toEnum $ (enumMax + 1) * enumMax - 1
      where
        enumMax = fromEnum (maxBound :: a)