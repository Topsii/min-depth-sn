{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language GeneralizedNewtypeDeriving #-}

module MinDepthSN.Data.Size 
    ( 
    -- * Channel
      Channel
    , n
    , channels
    , channelsBefore
    , channelsAfter
    , channelsBetween
    -- * Layer
    , Layer
    , d
    , layers
    -- * BetweenLayers
    , BetweenLayers
    , before
    , after
    , beforeFirstLayer
    , afterLastLayer
    , beforeLayers
    , afterLayers
    -- * Gate
    , Gate
    ) where

import Data.Finite (Finite, weaken, shiftN)
import GHC.TypeNats (KnownNat, Div, type (+))
import Enumerate
    ( Enumerable
    , enumerated
    , boundedEnumerated
    , cardinality
    , boundedCardinality
    )

n :: Int
n = length channels

d:: Int
d = length layers

channels :: [Channel]
channels = [ minBound .. maxBound ]

channelsBefore :: Channel -> [Channel]
channelsBefore c
    | c == minBound = []
    | otherwise = [ minBound .. pred c ]

channelsAfter :: Channel -> [Channel]
channelsAfter c
    | c == maxBound = []
    | otherwise = [ succ c .. maxBound ]

-- |    channelsBetween a b == [a+1, a+2, .. b-2, b-1]
channelsBetween :: Channel -> Channel -> [Channel]
channelsBetween from to
    | from == maxBound = []
    | to == minBound = []
    | otherwise = [ succ from .. pred to ]

layers :: [Layer]
layers = [ 0 .. maxBound ]

before :: Layer -> BetweenLayers
before (Layer k) = BetweenLayers $ weaken k

after :: Layer -> BetweenLayers
after (Layer k) = BetweenLayers $ shiftN k

beforeFirstLayer :: BetweenLayers
beforeFirstLayer = before minBound

afterLastLayer :: BetweenLayers
afterLastLayer = after maxBound

afterLayers :: [BetweenLayers]
afterLayers = map after layers

beforeLayers :: [BetweenLayers]
beforeLayers = map before layers

type N = 4
type D = 3

newtype Channel = Channel (Finite N)
    deriving
    ( Bounded
    , Enum
    , Eq
    , Integral -- ^ __Not__ modular arithmetic.
    , Num      -- ^ Modular arithmetic. Only the fromInteger function 
               -- is supposed to be useful.
    , Ord
    , Real)

newtype Layer = Layer (Finite D)
    deriving
    ( Bounded
    , Enum
    , Eq
    , Integral -- ^ __Not__ modular arithmetic.
    , Num      -- ^ Modular arithmetic. Only the fromInteger function 
               -- is supposed to be useful.
    , Ord
    , Real)

newtype Gate = Gate (Finite (N `Div` 2))
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
newtype BetweenLayers = BetweenLayers (Finite (D+1))
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
instance (Show Gate) where
    show (Gate a) = show $ toInteger a

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

instance Enumerable Gate where
    enumerated = boundedEnumerated
    cardinality = boundedCardinality