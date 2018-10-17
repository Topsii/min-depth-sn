{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MinDepthSN.Data.Combinatorics2.VariationNoRepetition
    ( VariationNoRepetition(VariationNoRepetition)
    ) where

import Data.Enum (boundedEnumFrom, boundedEnumFromThen)
import Control.Exception (assert)

data VariationNoRepetition a = MkVariationNoRepetition a a
    deriving (Eq, Ord, Show)

{-# COMPLETE VariationNoRepetition #-}
pattern VariationNoRepetition :: (Eq a, Show a) => a -> a -> VariationNoRepetition a
pattern VariationNoRepetition a a' <- MkVariationNoRepetition a a'
  where
    VariationNoRepetition a a' = assert (a /= a') $ MkVariationNoRepetition a a'

instance (Bounded a, Enum a, Eq a, Show a) => Enum (VariationNoRepetition a) where
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

instance (Bounded a, Enum a, Eq a, Show a) => Bounded (VariationNoRepetition a) where
    minBound = toEnum 0
    maxBound = toEnum $ (enumMax + 1) * enumMax - 1
      where
        enumMax = fromEnum (maxBound :: a)