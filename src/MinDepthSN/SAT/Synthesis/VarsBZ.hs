{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}

module MinDepthSN.SAT.Synthesis.VarsBZ
    ( module Size
    , NetworkSynthesis(..)
    , StandardNetworkSynthesis
    , GeneralizedNetworkSynthesis
    ) where

import SAT.IPASIR (AsVar(..))

import Numeric.Natural
import MinDepthSN.Data.Size (Channel, Layer, BetweenLayers)
import MinDepthSN.Data.GateOrUnused 
    ( GateOrUnused(..)
    , SortingOrder(..)
    , SortOrder
    )
import MinDepthSN.Data.Value (Value(..))
import MinDepthSN.Data.Unused (Unused)
import MinDepthSN.Data.Gate (Gate)

import qualified MinDepthSN.Data.Size as Size

import MinDepthSN.Data.Combinatorics2.CombinationNoRepetition
import MinDepthSN.Data.Combinatorics2.VariationNoRepetition

-- type StandardNetworkSynthesis = NetworkSynthesis 'Standard
-- type GeneralizedNetworkSynthesis = NetworkSynthesis 'Generalized

type StandardNetworkSynthesis = NetworkSynthesis CombinationNoRepetition
type GeneralizedNetworkSynthesis = NetworkSynthesis VariationNoRepetition

data NetworkSynthesis f
    = GateOrUnused_ { unGateOrUnused_ :: GateOrUnused f }
    | Value_ { counterExIdx :: Natural, unValue_ :: Value }
    deriving (Eq, Ord)

instance Show (NetworkSynthesis f) where
    show var = case var of
        GateOrUnused_ gu -> show gu
        Value_ cexIdx val -> show cexIdx ++ " " ++ show val

instance AsVar (NetworkSynthesis f) Unused where
    asVar = GateOrUnused_ . Unused_

instance AsVar (NetworkSynthesis f) (Gate f) where
    asVar = GateOrUnused_ . Gate_

instance AsVar (NetworkSynthesis f) (GateOrUnused f) where
    asVar = GateOrUnused_

instance AsVar (NetworkSynthesis f) (Natural, Value) where
    asVar = uncurry Value_

instance AsVar (NetworkSynthesis f) (NetworkSynthesis f) where
    asVar = id
            
-- | NetworkSynthesis values are enumerated as follows starting from 0:
-- where
--
--    0   <-> Gate_ minBound
--    ... <-> ...
--    ... <-> Gate_ maxBound
--    ... <-> Unused_ minBound
--    ... <-> ...
--    ... <-> Unused_ maxBound
--    ... <-> Value_ 0 minBound
--    ... <-> ...
--    ... <-> Value_ 0 maxBound
--    ... <-> Value_ 1 minBound
--    ... <-> ...
--    ... <-> Value_ 1 maxBound
--    ... <-> ...
instance Enum (NetworkSynthesis f) where

    toEnum i
        | i < 0 = error $ "toEnum (NetworkSynthesis): negative argument " ++ show i
        | i <= fromEnum (GateOrUnused_ maxBound :: NetworkSynthesis o) = GateOrUnused_ $ toEnum (i - gateOrUnusedOffset)
        | otherwise = Value_ (toEnum iter) (toEnum iVal)
        -- check if i < (2^n) * fromEnum (maxBound :: Value)
      where
        iter, iVal :: Int
        (iter, iVal) = (i - valueOffset) `quotRem` fromEnum (maxBound :: Value)
        gateOrUnusedOffset :: Int
        gateOrUnusedOffset = 0
        valueOffset :: Int
        valueOffset = fromEnum (GateOrUnused_ maxBound :: NetworkSynthesis o) + 1

    fromEnum var = case var of
        GateOrUnused_ gu -> gateOrUnusedOffset + fromEnum gu
        Value_ iter val  -> valueOffset  + fromEnum val +
            fromEnum iter * (fromEnum (maxBound :: Value) + 1)
      where
        gateOrUnusedOffset :: Int
        gateOrUnusedOffset = 0
        valueOffset :: Int
        valueOffset = fromEnum (GateOrUnused_ maxBound :: NetworkSynthesis o) + 1
