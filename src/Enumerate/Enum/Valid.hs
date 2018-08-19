{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language GeneralizedNewtypeDeriving #-}

module Enumerate.Enum.Valid where

import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
-- import Data.IntMap.Strict (IntMap)
-- import qualified Data.IntMap.Strict as IMap
import Data.Array (Array)
import qualified Data.Array as Array -- IntMap instead?

import Enumerate (Enumerable, enumerated, cardinality, boundedCardinality)
import Enumerate.Enum (toEnum_enumerable, fromEnum_enumerable)

class Validatable a where
    isValid :: a -> Bool

newtype Valid a = Valid a

deriving instance Eq a => Eq (Valid a)
deriving instance Ord a => Ord (Valid a)
deriving instance Show a => Show (Valid a)
deriving instance Validatable a => Validatable (Valid a)

instance (Enumerable a, Validatable a, Ord a) => Enumerable (Valid a) where
    enumerated = validEnumerated
    cardinality = boundedCardinality

instance (Enumerable a, Validatable a, Ord a) => Bounded (Valid a) where
    minBound = validMinBound
    maxBound = validMaxBound

instance (Enumerable a, Validatable a, Ord a) => Enum (Valid a) where
    toEnum   = toEnum_enumerable   arrayEnumerable
    fromEnum = fromEnum_enumerable tableEnumerable

validEnumerated :: (Enumerable a, Validatable a, Ord a) => [Valid a]
validEnumerated = map Valid . sort . filter isValid $ enumerated

arrayEnumerable :: forall a. (Enumerable a, Validatable a, Ord a) => Array Int (Valid a) --TODO
arrayEnumerable = Array.listArray (0, length valids - 1) valids --TODO is array efficient?
  where
    valids :: [Valid a]
    valids = validEnumerated

tableEnumerable :: forall a. (Enumerable a, Ord a, Validatable a) => Map (Valid a) Int
tableEnumerable = Map.fromList (zip valids [0 .. length valids - 1])
  where
    valids :: [Valid a]
    valids = validEnumerated

validMaxBound :: (Enumerable a, Ord a, Validatable a) => (Valid a)
validMaxBound = maximum validEnumerated

validMinBound :: (Enumerable a, Ord a, Validatable a) => (Valid a)
validMinBound = minimum validEnumerated
