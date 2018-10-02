{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}

module MinDepthSN.SAT.Synthesis.VarsHaslop where

import SAT.IPASIR (AsVar(..))

import MinDepthSN.Data.GateIn
import MinDepthSN.Data.GateOut

data NetworkGraphSynthesis
    = GateIn_ GateIn
    | GateOut_ GateOut

instance Show NetworkGraphSynthesis where
    show var = case var of
        GateIn_ gi -> show gi
        GateOut_ go -> show go

instance AsVar NetworkGraphSynthesis GateIn where
    asVar = GateIn_

instance AsVar NetworkGraphSynthesis GateOut where
    asVar = GateOut_

instance AsVar NetworkGraphSynthesis NetworkGraphSynthesis where
    asVar = id

gateInOffset :: Int
gateInOffset = 0

gateOutOffset :: Int
gateOutOffset = fromEnum (GateIn_ maxBound) + 1

instance Enum NetworkGraphSynthesis where
    toEnum i
        | i < 0 = error $ "toEnum (NetworkGraphSynthesis): negative argument " ++ show i
        | i <= fromEnum (GateIn_ maxBound) = GateIn_ $ toEnum (i - gateInOffset)
        | i <= fromEnum (GateOut_ maxBound) = GateOut_ $ toEnum (i - gateOutOffset)
        | otherwise = error $ "toEnum (NetworkGraphSynthesis): argument too big " ++ show i

    fromEnum var = case var of
        GateIn_ gi -> gateInOffset + fromEnum gi
        GateOut_ go -> gateOutOffset + fromEnum go