{-# language DerivingVia #-}
{-# language DeriveGeneric #-}

module MinDepthSN.Data.Wire where

import MinDepthSN.Data.Size

import MinDepthSN.Data.Combinatorics2.CombinationNoRepetition

import Generic.Data
import GHC.Generics


-- data Wire = Wire FromPin ToPin

-- data FromPin = In Channel | From UsedChannel Layer

-- data ToPin = To UsedChannel Layer | Out Channel


-- data Sorted = Sorted (CombinationOfTwo Channel) BetweenLayers BetweenLayers

data Pin
    = In Channel
    | Inner UsedChannel Layer
    | Out Channel

data Wire = Wire Channel Channel (CombinationNoRepetition BetweenLayers)
    deriving (Eq, Ord, Generic)
    deriving Bounded via (Generically Wire)
    deriving Enum via (FiniteEnumeration Wire)

-- pattern Wire :: Channel -> Channel -> Layer -> Layer
-- pattern Wire i j k l = 

instance Show Wire where
    show (Wire i j (CombinationNoRepetition k l)) = "Wire " ++ show i ++ " " ++ show j ++ " " ++ show k ++ " " ++ show l

-- FromChan ToChan FromLayer ToLayer
-- 0 0 0 1
-- 0 1 0 1
-- 1 0 0 1
-- 1 1 0 1
-- 2 0 0 1
-- 2 1 0 1
-- 0 0 0 2
-- 0 1 0 2
-- 1 0 0 2
-- 1 1 0 2
-- 2 0 0 2
-- 2 1 0 2
-- 0 0 1 2
-- 0 1 1 2
-- 1 0 1 2
-- 1 1 1 2
-- 0 0 0 3
-- 0 1 0 3
-- 1 0 0 3
-- 1 1 0 3
-- 2 0 0 3
-- 2 1 0 3
-- 0 0 1 3
-- 0 1 1 3
-- 1 0 1 3
-- 1 1 1 3
-- 0 0 2 3
-- 0 1 2 3
-- 1 0 2 3
-- 1 1 2 3
-- 0 0 0 4
-- 0 1 0 4
-- 0 2 0 4
-- 1 0 0 4
-- 1 1 0 4
-- 1 2 0 4
-- 2 0 0 4
-- 2 1 0 4
-- 2 2 0 4
-- 0 0 1 4
-- 0 1 1 4
-- 0 2 1 4
-- 1 0 1 4
-- 1 1 1 4
-- 1 2 1 4
-- 0 0 2 4
-- 0 1 2 4
-- 0 2 2 4
-- 1 0 2 4
-- 1 1 2 4
-- 1 2 2 4
-- 0 0 3 4
-- 0 1 3 4
-- 0 2 3 4
-- 1 0 3 4
-- 1 1 3 4
-- 1 2 3 4


-- data Wire
--     = InWire
--         { fromIn :: Channel
--         , toChannel :: UsedChannel
--         , toLayer :: Layer
--         }
--     | Wire 
--         { fromChannel :: UsedChannel
--         , fromLayer :: Layer
--         , toChannel :: UsedChannel
--         , toLayer :: Layer
--         }
--     | OutWire
--         { fromChannel :: UsedChannel
--         , fromLayer :: Layer
--         , toOut :: Channel
--         }
--     deriving (Eq, Ord, Generic)
--     deriving Bounded via (Generically Wire)
--     deriving Enum via (FiniteEnumeration Wire)

-- instance Show Wire where
--     show w = case w of
--         InWire  i   a h -> "InWire "  ++ show i ++ " " ++ show a ++ " " ++ show h
--         Wire    i k a h -> "Wire "    ++ show i ++ " " ++ show k ++ " " ++ show a ++ " " ++ show h
--         OutWire i k a   -> "OutWire " ++ show i ++ " " ++ show k ++ " " ++ show a