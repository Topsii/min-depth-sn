{-# language DataKinds #-}
{-# language GeneralizedNewtypeDeriving #-}

{-# language ExistentialQuantification #-}
{-# language TypeOperators #-}

module MinDepthSN.Data.Size where

import Data.Finite --(Finite)
import GHC.TypeNats-- (KnownNat)
import Enumerate
    ( Enumerable
    , enumerated
    , boundedEnumerated
    , cardinality
    , boundedCardinality
    )

n :: Int
n = length layers

d:: Int
d = length channels

channels :: [Channel]
channels = [ minBound .. maxBound ]

channelsAfter :: Channel -> [Channel]
channelsAfter c
    | c == maxBound = []
    | otherwise = [ succ c .. maxBound ]

channelsBefore :: Channel -> [Channel]
channelsBefore c
    | c == minBound = []
    | otherwise = [ minBound .. pred c ]

beforeFirstLayer :: BetweenLayers
beforeFirstLayer = before minBound

afterLastLayer :: BetweenLayers
afterLastLayer = after maxBound

layers :: [Layer]
layers = [ 0 .. maxBound ]

afterLayers :: [BetweenLayers]
afterLayers = map after layers

after :: Layer -> BetweenLayers
after (Layer k) = BetweenLayers $ shiftN k

beforeLayers :: [BetweenLayers]
beforeLayers = map before layers

before :: Layer -> BetweenLayers
before (Layer k) = BetweenLayers $ weaken k

newtype Channel = Channel (Finite 4)
    deriving
    ( Bounded
    , Enum
    , Eq
    , Integral -- ^ __Not__ modular arithmetic.
    , Num      -- ^ Modular arithmetic. Only the fromInteger function 
               -- is supposed to be useful.
    , Ord
    , Real)

newtype Layer = Layer (Finite 3)
    deriving
    ( Bounded
    , Enum
    , Eq
    , Integral -- ^ __Not__ modular arithmetic.
    , Num      -- ^ Modular arithmetic. Only the fromInteger function 
               -- is supposed to be useful.
    , Ord
    , Real)

-- | Value at some chan, between some layers.
--
-- A value of 0 indicates an input value of the network.
--
-- A value of d is an output value of the network.
newtype BetweenLayers = BetweenLayers (Finite 4)
    deriving
    ( Bounded
    , Enum
    , Eq
    , Integral -- ^ __Not__ modular arithmetic.
    , Num      -- ^ Modular arithmetic. Only the fromInteger function 
               -- is supposed to be useful.
    , Ord
    , Real)

instance (Show Channel) where
    show (Channel i) = show $ toInteger i
instance (Show Layer) where
    show (Layer k) = show $ toInteger k
instance (Show BetweenLayers) where
    show (BetweenLayers k) = show $ toInteger k

instance KnownNat n => Enumerable (Finite n) where
    enumerated = boundedEnumerated
    cardinality = boundedCardinality

instance Enumerable Channel where
    enumerated = boundedEnumerated
    cardinality = boundedCardinality

instance Enumerable Layer where
    enumerated = boundedEnumerated
    cardinality = boundedCardinality

instance Enumerable BetweenLayers where
    enumerated = boundedEnumerated
    cardinality = boundedCardinality