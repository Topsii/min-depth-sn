{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language ScopedTypeVariables #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language DerivingStrategies #-}
{-# language DerivingVia #-}
{-# language DeriveGeneric #-}

module MinDepthSN.SAT.Synthesis.VarsBZ
    ( module Size
    , NetworkSynthesis(..)
    ) where

import SAT.IPASIR (AsVar(..))

import Data.Word (Word32)
import Generic.Data
import MinDepthSN.Data.Size (Channel, Layer, BetweenLayers)
import MinDepthSN.Data.GateOrUnused()
import MinDepthSN.Data.Value (Value(..))
import MinDepthSN.Data.Unused (Unused)
import MinDepthSN.Data.Gate (Gate)

import qualified MinDepthSN.Data.Size as Size

data NetworkSynthesis
    = Gate_ { unGate :: Gate }
    | Unused_ { unUnused :: Unused }
    | Value_ { counterExIdx :: Word32, unValue_ :: Value }
    deriving stock (Eq, Ord, Generic)
    deriving Enum via (FiniteEnumeration NetworkSynthesis)

instance Show NetworkSynthesis where
    show var = case var of
        Gate_ g -> show g
        Unused_ u -> show u
        Value_ cexIdx val -> show cexIdx ++ " " ++ show val

instance AsVar NetworkSynthesis Unused where
    asVar = Unused_

instance AsVar NetworkSynthesis Gate where
    asVar = Gate_

instance AsVar NetworkSynthesis (Either Gate Unused) where
    asVar = either Gate_ Unused_

instance AsVar NetworkSynthesis (Word32, Value) where
    asVar = uncurry Value_

instance AsVar NetworkSynthesis NetworkSynthesis where
    asVar = id
            