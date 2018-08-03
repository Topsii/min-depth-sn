{-# language ScopedTypeVariables #-}

module Enumerate.Enum.Valid where

import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
-- import Data.IntMap.Strict (IntMap)
-- import qualified Data.IntMap.Strict as IMap
import Data.Array (Array)
import qualified Data.Array as Array -- IntMap instead?

import Enumerate (Enumerable, enumerated)

class Validatable a where
    isValid :: a -> Bool

validEnumerated :: (Enumerable a, Validatable a, Ord a) => [a]
validEnumerated = sort (filter isValid enumerated)

arrayEnumerable :: forall a. (Enumerable a, Validatable a, Ord a) => Array Int a --TODO
arrayEnumerable = Array.listArray (0, length valids - 1) valids --TODO is array efficient?
  where
    valids :: [a]
    valids = validEnumerated

tableEnumerable :: forall a. (Enumerable a, Ord a, Validatable a) => Map a Int
tableEnumerable = Map.fromList (zip valids [0 .. length valids - 1])
  where
    valids :: [a]
    valids = validEnumerated

validMaxBound :: (Enumerable a, Ord a, Validatable a) => a
validMaxBound = maximum validEnumerated 

validMinBound :: (Enumerable a, Ord a, Validatable a) => a
validMinBound = minimum validEnumerated 
