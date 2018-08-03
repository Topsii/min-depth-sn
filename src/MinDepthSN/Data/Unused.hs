{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}

module MinDepthSN.Data.Unused
    ( Unused(Unused)
    ) where

import Data.Ord (comparing)
import Data.Monoid ((<>))
import Data.Map.Strict (Map)
import Data.Array (Array)
import GHC.Generics (Generic)
import Enumerate (Enumerable)
import Enumerate.Enum (toEnum_enumerable, fromEnum_enumerable)
import Enumerate.Enum.Valid (Validatable, isValid, tableEnumerable, arrayEnumerable, validMaxBound, validMinBound)
import MinDepthSN.Data.Size

data Unused = Unused { channel :: Channel, layer :: Layer }
    deriving (Eq, Generic, Enumerable)

instance Show Unused where
    show (Unused i k) = "Unused " ++ show i ++ " " ++ show k

instance Ord Unused where
    compare = comparing layer <> comparing channel

instance Bounded Unused where
    minBound = validMinBound
    maxBound = validMaxBound

instance Validatable Unused where
    isValid = const True

instance Enum Unused where
    toEnum   = toEnum_enumerable   arrayUnused
    fromEnum = fromEnum_enumerable tableUnused

tableUnused :: Map Unused Int
tableUnused = tableEnumerable

arrayUnused :: Array Int Unused
arrayUnused = arrayEnumerable