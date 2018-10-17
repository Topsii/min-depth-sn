{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MinDepthSN.Data.Combinatorics2.CombinationNoRepetition
    ( CombinationNoRepetition(CombinationNoRepetition)
    ) where

import Data.Enum (boundedEnumFrom, boundedEnumFromThen)
import Control.Exception (assert)

data CombinationNoRepetition a = MkCombinationNoRepetition a a
    deriving (Eq, Ord, Show)

{-# COMPLETE CombinationNoRepetition #-}
pattern CombinationNoRepetition :: (Ord a, Show a) => a -> a -> CombinationNoRepetition a
pattern CombinationNoRepetition a a' <- MkCombinationNoRepetition a a'
  where
    CombinationNoRepetition a a' = assert (a < a') $ MkCombinationNoRepetition a a'

toTriangular :: Int -> Int
toTriangular n = (n * (n + 1)) `div` 2

fromTriangular :: Int -> Int
fromTriangular tri_n = floor $ (sqrt (2 * float_tri_n + 0.25)) - 0.5
  where
    float_tri_n :: Double
    float_tri_n = fromIntegral tri_n

instance (Bounded a, Enum a, Ord a, Show a) => Enum (CombinationNoRepetition a) where
    fromEnum (CombinationNoRepetition x1 x2) =
        i_x1 * enumMax - toTriangular i_x1 - 1 + i_x2
      where
        i_x1, i_x2, enumMax :: Int
        i_x1 = fromEnum x1
        i_x2 = fromEnum x2
        enumMax = fromEnum (maxBound :: a)
    toEnum n = CombinationNoRepetition (toEnum i_x1) (toEnum i_x2)
      where
        i_x1, i_x2, enumMax, cardA, cardCombNoRep :: Int
        i_x1 = cardA - (2 + fromTriangular (cardCombNoRep - n))
        i_x2 = n - (i_x1 * enumMax - toTriangular i_x1 - 1)
        enumMax = fromEnum (maxBound :: a)
        cardA = enumMax + 1
        cardCombNoRep = toTriangular (cardA - 1) - 1
    enumFrom = boundedEnumFrom
    enumFromThen = boundedEnumFromThen

instance (Bounded a, Enum a, Ord a, Show a) => Bounded (CombinationNoRepetition a) where
    minBound = toEnum 0
    maxBound = toEnum . subtract 1 . toTriangular $ fromEnum (maxBound :: a)