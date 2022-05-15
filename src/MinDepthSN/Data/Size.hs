{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoStarIsType #-}

module MinDepthSN.Data.Size 
    ( 
    -- * Channel
      Channel
    , n
    , N
    , firstChannel
    , lastChannel
    , channels
    , areAdjacent
    -- * Layer
    , Layer
    , d
    , D
    , firstLayer
    , lastLayer
    , layers
    -- * BetweenLayers
    , BetweenLayers
    , before
    , after
    , beforeFirstLayer
    , afterLastLayer
    , beforeLayers
    , afterLayers
    -- -- * GateInLayer
    -- , GateInLayer
    -- , gatesInLayer
    -- , maxForwardChannel
    -- , minForwardChannel
    -- * Used Channel
    , UsedChannel
    ) where

-- import Control.Monad (join)
-- import Data.Maybe (fromJust)
import Data.Finite
  ( Finite
  , weaken
  , shiftN
--   , weakenN
--   , shift
--   , add
--   , strengthen
  )
import GHC.TypeNats (Div, type (+), type (*), KnownNat)
-- import GHC.TypeNats (Nat)
import GHC.Arr

import GHC.Generics (Generic)

-- data ChannelCnt where
--     MkChannelCnt :: Nat -> ChannelCnt

-- type family ChannelCntDotNat (n :: ChannelCnt) where
--     ChannelCntDotNat ('MkChannelCnt nat) = nat

-- class (KnownNat (ChannelCntDotNat n)) => KnownChannelCnt n

-- data LayerCnt where
--     MkLayerCnt :: Nat -> LayerCnt
--     deriving stock Show

-- instance Show Nat where
--     show _ = "Ha"

-- type family LayerCntDotNat (d :: LayerCnt) :: Nat where
--     LayerCntDotNat ('MkLayerCnt nat) = nat

-- class (KnownNat (LayerCntDotNat d)) => KnownLayerCnt d

-- data NetSize where
--     MkSize :: ChannelCnt -> LayerCnt -> NetSize

-- type family SizeDotChannelCnt (s :: NetSize) :: ChannelCnt where
--     SizeDotChannelCnt ('MkSize n _d) = n

-- type family SizeDotLayerCnt (s :: NetSize) :: LayerCnt where
--     SizeDotLayerCnt ('MkSize _n d) = d

-- class (KnownChannelCnt (SizeDotChannelCnt s), KnownLayerCnt (SizeDotLayerCnt s)) => KnownNetSize s










-- -- class (KnownNat (GetChannelCnt a)) => KnownChannelCnt2 (a :: k) where
-- --     type GetChannelCnt :: k -> Nat

-- -- instance KnownChannelCnt2 NetSize where
-- --     type GetChannelCnt ('MkSize n _d) n 

-- class (KnownNat (GetLayerCnt a)) => KnownLayerCnt2 (a :: k) where
--     type GetLayerCnt a :: Nat

-- instance KnownLayerCnt2 (f ::) where
--     type GetLayerCnt ('MkLayerCnt d) = d 
--     -- type GetLayerCnt a = LayerCntDotNat a

-- -- instance KnownLayerCnt2 LayerCnt where
-- --     -- type GetLayerCnt ('MkLayerCnt d) = d 
-- --     type GetLayerCnt a = LayerCntDotNat a

-- instance KnownLayerCnt2 NetSize where
--     type GetLayerCnt ('MkSize _n (MkLayerCnt d)) = d 

-- -- class (KnownChannelCnt2 s, KnownLayerCnt2 s) => KnownNetSize2 s





n :: Int
n = length channels

d:: Int
d = length layers

areAdjacent :: Channel -> Channel -> Bool
areAdjacent i j = abs (fromIntegral i - fromIntegral j :: Int) == 1

firstChannel :: Channel
firstChannel = minBound

lastChannel :: Channel
lastChannel = maxBound

channels :: [Channel]
channels = [ minBound .. maxBound ]

firstLayer :: Layer
firstLayer = minBound

lastLayer :: Layer
lastLayer = maxBound

layers :: [Layer]
layers = [ minBound .. maxBound ]

before :: Layer -> BetweenLayers
before (Layer k) = BetweenLayers $ weaken k

after :: Layer -> BetweenLayers
after (Layer k) = BetweenLayers $ shiftN k

beforeFirstLayer :: BetweenLayers
beforeFirstLayer = before firstLayer

afterLastLayer :: BetweenLayers
afterLastLayer = after lastLayer

afterLayers :: [BetweenLayers]
afterLayers = map after layers

beforeLayers :: [BetweenLayers]
beforeLayers = map before layers

-- minForwardChannel :: GateInLayer -> Channel
-- minForwardChannel (GateInLayer g) = Channel . weakenIfOddN . timesTwo $ g

-- maxForwardChannel :: GateInLayer -> Channel
-- maxForwardChannel (GateInLayer g) = Channel . weakenIfOddN . timesTwoPlusOne $ g

-- weakenIfOddN :: Finite ((N `Div` 2) * 2) -> Finite N
-- weakenIfOddN = weakenN

-- timesTwo :: Finite n -> Finite (n + n)
-- timesTwo = join add

-- there is no such KnownNat m for n ~ 0, thus maxForwardChannel does not work 
-- for N = 0 or N = 1, since (N `Div` 2) ~ 0 in thoses cases.
-- timesTwoPlusOne :: (KnownNat m, KnownNat n, (n+n) ~ (m+1)) => Finite n -> Finite (n + n)
-- timesTwoPlusOne = shift . fromJust . strengthen . timesTwo

type N = 32
type D = 8

-- gatesInLayer :: [GateInLayer]
-- gatesInLayer = [ minBound .. maxBound ]

newtype Channel = Channel (Finite N)
    deriving newtype
    ( Bounded
    , Enum
    , Eq
    , Integral -- ^ __Not__ modular arithmetic.
    , Num      -- ^ Modular arithmetic. Only the fromInteger function 
               -- is supposed to be useful.
    , Ord
    , Ix
    , Real)
    deriving stock Generic

newtype Layer = Layer (Finite D)
    deriving newtype
    ( Bounded
    , Enum
    , Eq
    , Integral -- ^ __Not__ modular arithmetic.
    , Num      -- ^ Modular arithmetic. Only the fromInteger function 
               -- is supposed to be useful.
    , Ord
    , Ix
    , Real)
    deriving stock Generic


-- newtype GateInLayer = GateInLayer (Finite (N `Div` 2))
--     deriving newtype
--     ( Bounded
--     , Enum
--     , Eq
--     , Integral -- ^ __Not__ modular arithmetic.
--     , Num      -- ^ Modular arithmetic. Only the fromInteger function 
--                 -- is supposed to be useful.
--     , Ord
--     , Ix
--     , Real)
--     deriving stock Generic

newtype UsedChannel = UsedChannel (Finite ((N `Div` 2) * 2))
    deriving newtype
    ( Bounded
    , Enum
    , Eq
    , Integral -- ^ __Not__ modular arithmetic.
    , Num      -- ^ Modular arithmetic. Only the fromInteger function 
                -- is supposed to be useful.
    , Ord
    , Ix
    , Real)
    deriving stock Generic

-- | Indicates a position between some layers.
--
-- A value of 0 indicates an input position (before the first layer).
--
-- A value of d indicates an output position (after the last layer).
newtype BetweenLayers = BetweenLayers (Finite (D+1))
    deriving newtype
    ( Bounded
    , Enum
    , Eq
    , Integral -- ^ __Not__ modular arithmetic.
    , Num      -- ^ Modular arithmetic. Only the fromInteger function 
               -- is supposed to be useful.
    , Ord
    , Ix
    , Real)
    deriving stock Generic


instance (Show Channel) where
    showsPrec _p (Channel i) = showsPrec 11 (toInteger i)
instance (Show Layer) where
    showsPrec _p (Layer k) = showsPrec 11 (toInteger k)
instance (Show BetweenLayers) where
    showsPrec _p (BetweenLayers k) = showsPrec 11 (toInteger k)
-- instance (Show GateInLayer) where
--     showsPrec p (GateInLayer a) = showsPrec 11 (toInteger a)
instance (Show UsedChannel) where
    showsPrec _p (UsedChannel i) = showsPrec 11 (toInteger i)


instance KnownNat n => Ix (Finite n) where
    range (l, h) = [l .. h]
    unsafeIndex (l, _) i = fromIntegral (i - l)
    inRange (l, h) i = l <= i && i <= h