{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# language FlexibleContexts #-}

module MinDepthSN.Data.GateOut
    ( GateOut(GateOut)
    , gateOutLit
    , UnvalidatedGateOut(UnvalidatedGateOut)
    ) where

import Data.Ord (comparing)
import Data.Monoid ((<>))
import SAT.IPASIR (AsVar(..), Lit, lit)
import MinDepthSN.Data.Size (GateInLayer, Layer, n, d)
import GHC.Generics (Generic)
import Enumerate
    ( Enumerable
    , enumerated
    , boundedEnumerated
    , cardinality
    , boundedCardinality
    )
import Enumerate.Enum.Valid (Validatable, Valid(..), isValid)


import Enumerate.Enum
    ( toEnum_enumerable
    -- , fromEnum_enumerable
    )
import Data.List (sort)
-- import Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as Map
-- import Data.IntMap.Strict (IntMap)
-- import qualified Data.IntMap.Strict as IMap
import Data.Array (Array)
import qualified Data.Array as Array -- IntMap instead?
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable


data UnvalidatedGateOut = UnvalidatedGateOut {
    minForwardGate :: GateInLayer,
    maxForwardGate :: GateInLayer,
    gate :: GateInLayer,
    minForwardLayer :: Layer,
    maxForwardLayer :: Layer,
    layer :: Layer
} deriving (Eq, Generic, Enumerable, Show)

instance Hashable UnvalidatedGateOut

instance Ord UnvalidatedGateOut where
    compare = 
        comparing layer <> 
        comparing minForwardLayer <> 
        comparing maxForwardLayer <> 
        comparing gate <> 
        comparing minForwardGate <> 
        comparing maxForwardGate


instance Validatable UnvalidatedGateOut where
    isValid UnvalidatedGateOut {..} =
        layer < minForwardLayer && layer < maxForwardLayer

newtype GateOut = ValidGateOut (Valid UnvalidatedGateOut)
    deriving newtype (Eq, Ord, Validatable, Show)
    deriving Generic
instance Hashable GateOut

validEnumerated :: [GateOut]
validEnumerated = map ValidGateOut . sort . filter isValid $ enumerated

arrayEnumerable :: Array Int GateOut --TODO
arrayEnumerable = Array.listArray (0, length valids - 1) valids --TODO is array efficient?
    where
    valids :: [GateOut]
    valids = validEnumerated

tableEnumerable :: HashMap GateOut Int
tableEnumerable = HashMap.fromList (zip valids [0 .. length valids - 1])
    where
    valids :: [GateOut]
    valids = validEnumerated

validMaxBound :: GateOut
validMaxBound = maximum validEnumerated

validMinBound :: GateOut
validMinBound = minimum validEnumerated

fromEnum_enumerable :: (Enumerable a, Ord a, Hashable a) => HashMap a Int -> (a -> Int)
fromEnum_enumerable as = \x -> (__fromJust__ "fromEnum") (HashMap.lookup x as)
{-# INLINE fromEnum_enumerable #-}

__fromJust__ :: String -> Maybe a -> a
__fromJust__ name = maybe (__bug__ name) id

__bug__ :: String -> a
__bug__ name = error (name ++ ": invalid Enumerable instance")
--TODO print typerep; add constraint, all types are Typeable

-- instance Enum GateOut where
--     toEnum _ = undefined --GateOut (toEnum i) (toEnum k)
--     --   where
--     --     (k,i) = int `quotRem` n
--     -- fromEnum (GateOut a b c x y z) = fromEnum x * (d^2) + fromEnum y * d + fromEnum z
--     fromEnum _ = 0
--     -- [n2, n2, n2, d, d, d][a,b,c,d,e,f]

n2 :: Int
n2 = n `div` 2

instance Enum GateOut where
    toEnum   = toEnum_enumerable   arrayEnumerable
    fromEnum = fromEnum_enumerable tableEnumerable

pattern GateOut :: GateInLayer -> GateInLayer -> GateInLayer -> Layer -> Layer -> Layer -> GateOut
pattern GateOut a b c x y z = ValidGateOut (Valid (UnvalidatedGateOut a b c x y z))

instance Bounded GateOut where
    minBound = validMinBound
    maxBound = validMaxBound

instance Enumerable GateOut where
    enumerated = boundedEnumerated
    cardinality = boundedCardinality

-- | Literal of 'GateOut' with positive polarity.
gateOutLit :: AsVar v GateOut => GateInLayer -> GateInLayer -> GateInLayer -> Layer -> Layer -> Layer -> Lit v
gateOutLit a b c x y z = lit (GateOut a b c x y z)
