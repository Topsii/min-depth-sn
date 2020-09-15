{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language ScopedTypeVariables #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language DerivingStrategies #-}

module MinDepthSN.SAT.Synthesis.VarsBZ
    ( module Size
    , NetworkSynthesis(..)
    ) where

import SAT.IPASIR (AsVar(..), Var(..), Dimacs)

import Generic.Data
import Data.Word (Word32)
import MinDepthSN.Data.Size (Channel, Layer, BetweenLayers)
import MinDepthSN.Data.GateOrUnused(GateOrUnused(..))
import MinDepthSN.Data.Value (Value(..))
import MinDepthSN.Data.Unused (Unused)
import MinDepthSN.Data.Gate (Gate, KnownNetType)

import qualified MinDepthSN.Data.Size as Size

data NetworkSynthesis t
    = GateOrUnused_ { unGateOrUnused_ :: GateOrUnused t }
    | Value_ { counterExIdx :: Word32, unValue_ :: Value }
    deriving stock (Generic, Eq, Ord)
    deriving Enum via (FiniteEnumeration (NetworkSynthesis t))
    deriving Dimacs via (Var (NetworkSynthesis t))

instance KnownNetType t => Show (NetworkSynthesis t) where
    show ns = case ns of
        GateOrUnused_ gu -> show gu
        Value_ cexIdx val -> show cexIdx ++ " " ++ show val

instance KnownNetType t => AsVar (NetworkSynthesis t) Unused where
    var = GateOrUnused_ . Unused_

instance KnownNetType t => AsVar (NetworkSynthesis t) (Gate t) where
    var = GateOrUnused_ . Gate_

instance KnownNetType t => AsVar (NetworkSynthesis t) (GateOrUnused t) where
    var = GateOrUnused_

instance KnownNetType t => AsVar (NetworkSynthesis t) (Word32, Value) where
    var = uncurry Value_

instance KnownNetType t => AsVar (NetworkSynthesis t) (NetworkSynthesis t) where
    var = id
            