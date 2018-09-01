{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TypeFamilies #-}

module MinDepthSN.Data.Size 
    ( preceding
    , succeeding
    , between
    -- * Channel
    , Channel
    , n
    , channels
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
    -- * GateInLayer
    , GateInLayer
    , gatesInLayer
    , maxForwardChannel
    , minForwardChannel
    ) where

import Control.Monad (join)
import Data.Maybe (fromJust)
import Data.Finite (Finite, weaken, shiftN, weakenN, shift, add, strengthen )
import GHC.TypeNats (Div, type (+), type (*), KnownNat)
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
channels = enumerated

-- | > preceding x == [ minBound .. pred x ]
preceding :: (Eq a, Bounded a, Enum a) => a -> [a]
preceding x
    | x == minBound = []
    | otherwise = [ minBound .. pred x ]

-- | > succeeding a == [ succ x .. maxBound ]
succeeding :: (Eq a, Bounded a, Enum a) => a -> [a]
succeeding x
    | x == maxBound = []
    | otherwise = [ succ x .. maxBound ]

-- | > between from to == [ succ from .. pred to ]
between :: (Eq a, Bounded a, Enum a) => a -> a -> [a]
between from to
    | from == maxBound = []
    | to == minBound = []
    | otherwise = [ succ from .. pred to ]

layers :: [Layer]
layers = enumerated

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

minForwardChannel :: GateInLayer -> Channel
minForwardChannel (GateInLayer g) = Channel . weakenIfOddN . timesTwo $ g

maxForwardChannel :: GateInLayer -> Channel
maxForwardChannel (GateInLayer g) = Channel . weakenIfOddN . timesTwoPlusOne $ g

weakenIfOddN :: Finite ((N `Div` 2) * 2) -> Finite N
weakenIfOddN = weakenN

timesTwo :: Finite n -> Finite (n + n)
timesTwo = join add

-- there is no such KnownNat m for n ~ 0, thus maxForwardChannel does not work 
-- for N = 0 or N = 1, since (N `Div` 2) ~ 0 in thoses cases.
timesTwoPlusOne :: (KnownNat m, KnownNat n, (n+n) ~ (m+1)) => Finite n -> Finite (n + n)
timesTwoPlusOne = shift . fromJust . strengthen . timesTwo

type N = 8
type D = 5

gatesInLayer :: [GateInLayer]
gatesInLayer = enumerated

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

newtype GateInLayer = GateInLayer (Finite (N `Div` 2))
    deriving
    ( Bounded
    , Enum
    , Eq
    , Integral -- ^ __Not__ modular arithmetic.
    , Num      -- ^ Modular arithmetic. Only the fromInteger function 
                -- is supposed to be useful.
    , Ord
    , Real)

newtype UsedChannel = UsedChannel (Finite ((N `Div` 2) * 2))
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
instance (Show GateInLayer) where
    show (GateInLayer a) = show $ toInteger a

instance Enumerable Channel where
    enumerated = boundedEnumerated
    cardinality = boundedCardinality

instance Enumerable Layer where
    enumerated = boundedEnumerated
    cardinality = boundedCardinality

instance Enumerable BetweenLayers where
    enumerated = boundedEnumerated
    cardinality = boundedCardinality

instance Enumerable GateInLayer where
    enumerated = boundedEnumerated
    cardinality = boundedCardinality