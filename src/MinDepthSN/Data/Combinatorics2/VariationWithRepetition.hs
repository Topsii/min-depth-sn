{-# language DerivingStrategies #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language ScopedTypeVariables #-}

module MinDepthSN.Data.Combinatorics2.VariationWithRepetition where

import Data.Ix (Ix(..))
import Generic.Data

data VariationWithRepetition a = VariationWithRepetition a a
    deriving stock (Generic, Eq, Ord, Show) -- change Show instance? Ix instance does not extendBounds like the other Selection types
    deriving Enum via (FiniteEnumeration (VariationWithRepetition a))
    deriving Bounded via (Generically (VariationWithRepetition a))

instance Ix a => Ix (VariationWithRepetition a) where
    range b =
        [
            VariationWithRepetition x y
        | x <- extendedRange
        , y <- extendedRange
        ]
      where
        extendedRange :: [a]
        extendedRange = range $ extendBounds b
    index b (VariationWithRepetition x y) = i_x * size + i_y 
      where
        i_x, i_y, size :: Int
        i_x = index extendedBounds x
        i_y = index extendedBounds y
        size = rangeSize extendedBounds
        extendedBounds :: (a, a)
        extendedBounds = extendBounds b
    inRange b (VariationWithRepetition x y) =
        inRange extendedBounds  x && inRange extendedBounds y
      where
        extendedBounds :: (a, a)
        extendedBounds = extendBounds b

extendBounds :: Ord a => (VariationWithRepetition a, VariationWithRepetition a) -> (a, a)
extendBounds (VariationWithRepetition x1 y1, VariationWithRepetition x2 y2) =
    (min x1 y1, max x2 y2)