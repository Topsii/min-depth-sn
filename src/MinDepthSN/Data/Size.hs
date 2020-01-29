{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoStarIsType #-}

module MinDepthSN.Data.Size 
    ( 
    -- * Channel
      Channel
    , n
    , firstChannel
    , lastChannel
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
    -- * Used Channel
    , UsedChannel
    ) where

import Control.Monad (join)
import Data.Maybe (fromJust)
import Data.Finite (Finite, weaken, shiftN, weakenN, shift, add, strengthen )
import GHC.TypeNats (Div, type (+), type (*), KnownNat)
import GHC.Arr

import GHC.Generics (Generic)

n :: Int
n = length channels

d:: Int
d = length layers

firstChannel :: Channel
firstChannel = minBound

lastChannel :: Channel
lastChannel = maxBound

channels :: [Channel]
channels = [ minBound .. maxBound ]

layers :: [Layer]
layers = [ minBound .. maxBound ]

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

type N = 12
type D = 8

gatesInLayer :: [GateInLayer]
gatesInLayer = [ minBound .. maxBound ]

newtype Channel = Channel (Finite N)
    deriving
    ( Bounded
    , Enum
    , Eq
    , Integral -- ^ __Not__ modular arithmetic.
    , Num      -- ^ Modular arithmetic. Only the fromInteger function 
               -- is supposed to be useful.
    , Ord
    , Ix
    , Generic
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
    , Ix
    , Generic
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
    , Ix
    , Generic
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
    , Ix
    , Generic
    , Real)

-- | Indicates a position between some layers.
--
-- A value of 0 indicates an input position (before the first layer).
--
-- A value of d indicates an output position (after the last layer).
newtype BetweenLayers = BetweenLayers (Finite (D+1))
    deriving
    ( Bounded
    , Enum
    , Eq
    , Integral -- ^ __Not__ modular arithmetic.
    , Num      -- ^ Modular arithmetic. Only the fromInteger function 
               -- is supposed to be useful.
    , Ord
    , Ix
    , Generic
    , Real)

-- | Indicates some layer or the input before the first layer or the output
-- after the last layer.
--
-- A value of 0 indicates an input position (before the first layer).
--
-- A value of 1 indicates the first layer.
--
-- A value of d indicates the last layer.
--
-- A value of d+1 indicates an output position (after the last layer).
newtype Level = Level (Finite (D+2))
    deriving
    ( Bounded
    , Enum
    , Eq
    , Integral -- ^ __Not__ modular arithmetic.
    , Num      -- ^ Modular arithmetic. Only the fromInteger function 
               -- is supposed to be useful.
    , Ord
    , Generic
    , Real)


instance (Show Channel) where
    show (Channel i) = show $ toInteger i
instance (Show Layer) where
    show (Layer k) = show $ toInteger k
instance (Show BetweenLayers) where
    show (BetweenLayers k) = show $ toInteger k
instance (Show GateInLayer) where
    show (GateInLayer a) = show $ toInteger a
instance (Show UsedChannel) where
    show (UsedChannel i) = show $ toInteger i


instance KnownNat n => Ix (Finite n) where
    range (l, h) = [l .. h]
    unsafeIndex (l, _) i = fromIntegral (i - l)
    inRange (l, h) i = l <= i && i <= h