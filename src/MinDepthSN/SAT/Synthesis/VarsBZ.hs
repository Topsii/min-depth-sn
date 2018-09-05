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

import SAT.IPASIR.EnumVarSolver (AsVar(..))

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

type StandardNetworkSynthesis = NetworkSynthesis 'Standard
type GeneralizedNetworkSynthesis = NetworkSynthesis 'Generalized

data NetworkSynthesis (o :: SortingOrder) 
    = GateOrUnused_ { unGateOrUnused_ :: GateOrUnused o }
    | Value_ { counterExIdx :: Natural, unValue_ :: Value }
    deriving (Eq, Ord)

instance SortOrder o => Show (NetworkSynthesis o) where
    show var = case var of
        GateOrUnused_ gu -> show gu
        Value_ cexIdx val -> show cexIdx ++ " " ++ show val

instance SortOrder o => AsVar (NetworkSynthesis o) Unused where
    v = GateOrUnused_ . Unused_

instance SortOrder o => AsVar (NetworkSynthesis o) (Gate o) where
    v = GateOrUnused_ . Gate_

instance SortOrder o => AsVar (NetworkSynthesis o) (GateOrUnused o) where
    v = GateOrUnused_

instance SortOrder o => AsVar (NetworkSynthesis o) (Natural, Value) where
    v = uncurry Value_

instance SortOrder o => AsVar (NetworkSynthesis o) (NetworkSynthesis o) where
    v = id
            
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
instance SortOrder o => Enum (NetworkSynthesis o) where

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
