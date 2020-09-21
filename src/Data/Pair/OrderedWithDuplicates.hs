{-# language DerivingStrategies #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language ScopedTypeVariables #-}

module Data.Pair.OrderedWithDuplicates where

import Data.Ix (Ix(..))
import Generic.Data

data OrderedWithDuplicates a = OrderedWithDuplicates a a
    deriving stock (Generic, Eq, Ord, Show) -- change Show instance? Ix instance does not extendBounds like the other Selection types
    deriving Enum via (FiniteEnumeration (OrderedWithDuplicates a))
    deriving Bounded via (Generically (OrderedWithDuplicates a))

instance Ix a => Ix (OrderedWithDuplicates a) where
    range b =
        [
            OrderedWithDuplicates x y
        | x <- extendedRange
        , y <- extendedRange
        ]
      where
        extendedRange :: [a]
        extendedRange = range $ extendBounds b
    index b (OrderedWithDuplicates x y) = i_x * size + i_y 
      where
        i_x, i_y, size :: Int
        i_x = index extendedBounds x
        i_y = index extendedBounds y
        size = rangeSize extendedBounds
        extendedBounds :: (a, a)
        extendedBounds = extendBounds b
    inRange b (OrderedWithDuplicates x y) =
        inRange extendedBounds x && inRange extendedBounds y
      where
        extendedBounds :: (a, a)
        extendedBounds = extendBounds b
    rangeSize b = aSize * aSize
      where
        aSize = rangeSize $ extendBounds b

extendBounds :: Ord a => (OrderedWithDuplicates a, OrderedWithDuplicates a) -> (a, a)
extendBounds (OrderedWithDuplicates x1 y1, OrderedWithDuplicates x2 y2) =
    (min x1 y1, max x2 y2)