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

import SAT.IPASIR (AsVar(..))

import Numeric.Natural
import MinDepthSN.Data.Size (Channel, Layer, BetweenLayers)
import MinDepthSN.Data.GateOrUnused()
import MinDepthSN.Data.Value (Value(..))
import MinDepthSN.Data.Unused (Unused)
import MinDepthSN.Data.Gate (Gate)

import qualified MinDepthSN.Data.Size as Size

data NetworkSynthesis
    = Gate_ { unGate :: Gate }
    | Unused_ { unUnused :: Unused }
    | Value_ { counterExIdx :: Natural, unValue_ :: Value }
    deriving stock (Eq, Ord)

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

instance AsVar NetworkSynthesis (Natural, Value) where
    asVar = uncurry Value_

instance AsVar NetworkSynthesis NetworkSynthesis where
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
instance Enum NetworkSynthesis where

    toEnum i
        | i < 0 = error $ "toEnum (NetworkSynthesis): negative argument " ++ show i
        | i <= fromEnum (Gate_ maxBound :: NetworkSynthesis) = Gate_ $ toEnum (i - gateOffset)
        | i <= fromEnum (Unused_ maxBound :: NetworkSynthesis) = Unused_ $ toEnum (i - unusedOffset)
        | otherwise = Value_ (toEnum iter) (toEnum iVal)
        -- check if i < (2^n) * fromEnum (maxBound :: Value)
      where
        iter, iVal :: Int
        (iter, iVal) = (i - valueOffset) `quotRem` fromEnum (maxBound :: Value)
        gateOffset, unusedOffset, valueOffset :: Int
        gateOffset = 0
        unusedOffset = fromEnum (Gate_ maxBound :: NetworkSynthesis) + 1
        valueOffset = fromEnum (Unused_ maxBound :: NetworkSynthesis) + 1

    fromEnum var = case var of
        Gate_ g -> gateOffset + fromEnum g
        Unused_ u -> unusedOffset + fromEnum u
        Value_ iter val  -> valueOffset  + fromEnum val +
            fromEnum iter * (fromEnum (maxBound :: Value) + 1)
      where
        gateOffset, unusedOffset, valueOffset :: Int
        gateOffset = 0
        unusedOffset = fromEnum (Gate_ maxBound :: NetworkSynthesis) + 1
        valueOffset = fromEnum (Unused_ maxBound :: NetworkSynthesis) + 1
