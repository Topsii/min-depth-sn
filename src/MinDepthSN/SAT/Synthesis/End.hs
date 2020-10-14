{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}

module MinDepthSN.SAT.Synthesis.End where

import Prelude hiding (negate)
import Data.Enum (succeeding)
import Data.Pair.AbsDiffGT1
import Data.Pair (Order(Unordered))
import SAT.IPASIR (Lit(..), negate)
import MinDepthSN.Vars


phi_1 :: [[Lit (NetworkSynthesis 'Standard)]]
phi_1 =
    [ [ -gateLit i j lastLayer ]
    | AbsDiffGT1 i j <- [ minBound .. maxBound ] :: [AbsDiffGT1 'Unordered Channel]
    ]

phi_1_l :: [[Lit (NetworkSynthesis 'Standard)]]
phi_1_l = 
    [ [ -gateLit i j l, -unusedLit i k, -unusedLit j k ] 
    | l <- layers
    , AbsDiffGT1 i j <- [ minBound .. maxBound ] :: [AbsDiffGT1 'Unordered Channel]
    , k <- succeeding l
    ]

phi_2 :: [[Lit (NetworkSynthesis 'Standard)]]
phi_2 =
    [ [ -gateLit i j (pred lastLayer) ]
    | i <- channels
    , j <- succeeding i
    , i + 3 < j --AbsDiffGT3
    ]

phi_3 :: [[Lit (NetworkSynthesis 'Standard)]]
phi_3 = concat
    [
        [ [ -gateLit i (i+3) (pred lastLayer), gateLit i     (i+1) lastLayer ]
        , [ -gateLit i (i+3) (pred lastLayer), gateLit (i+2) (i+3) lastLayer ]
        ]
    | i <- channels
    , i <= lastChannel - 3
    ]

phi_4 :: [[Lit (NetworkSynthesis 'Standard)]]
phi_4 =
    [
        [ -gateLit i (i+2) (pred lastLayer)
        , gateLit i     (i+1) lastLayer
        , gateLit (i+1) (i+2) lastLayer
        ]
    | i <- channels
    , i <= lastChannel - 2
    ]

psi_1 :: [[Lit (NetworkSynthesis 'Standard)]]
psi_1 =
    [ [ -unusedLit i lastLayer, -unusedLit (i+1) lastLayer ]
    | i <- channels
    , i < lastChannel
    ]

psi_2a :: [[Lit (NetworkSynthesis 'Standard)]]
psi_2a = concat
    [
        [   [ -gateLit i     (i+1) lastLayer
            , -gateLit (i+2) (i+3) lastLayer
            , -unusedLit i     (pred lastLayer)
            , -unusedLit (i+2) (pred lastLayer)
            ]
        ,   [ -gateLit i     (i+1) lastLayer
            , -gateLit (i+2) (i+3) lastLayer
            , -unusedLit i     (pred lastLayer)
            , -unusedLit (i+3) (pred lastLayer)
            ]
        ,   [ -gateLit i     (i+1) lastLayer
            , -gateLit (i+2) (i+3) lastLayer
            , -unusedLit (i+1) (pred lastLayer)
            , -unusedLit (i+2) (pred lastLayer)
            ]
        ,   [ -gateLit i     (i+1) lastLayer
            , -gateLit (i+2) (i+3) lastLayer
            , -unusedLit (i+1) (pred lastLayer)
            , -unusedLit (i+3) (pred lastLayer)
            ]
        ]
    | i <- channels
    , i <= lastChannel - 3
    ]

psi_2b :: [[Lit (NetworkSynthesis 'Standard)]]
psi_2b = concat
    [
        [   [ -gateLit i (i+1) lastLayer
            , -unusedLit (i+2) lastLayer
            , -unusedLit i     (pred lastLayer)
            , -unusedLit (i+2) (pred lastLayer)
            ]
        ,   [ -gateLit i (i+1) lastLayer
            , -unusedLit (i+2) lastLayer
            , -unusedLit (i+1) (pred lastLayer)
            , -unusedLit (i+2) (pred lastLayer)
            ]
        ]
    | i <- channels
    , i <= lastChannel - 2
    ]

psi_2c :: [[Lit (NetworkSynthesis 'Standard)]]
psi_2c = concat
    [
        [   [ -unusedLit i         lastLayer
            , -gateLit (i+1) (i+2) lastLayer
            , -unusedLit i         (pred lastLayer)
            , -unusedLit (i+1)     (pred lastLayer)
            ]
        ,   [ -unusedLit i         lastLayer
            , -gateLit (i+1) (i+2) lastLayer
            , -unusedLit i         (pred lastLayer)
            , -unusedLit (i+2)     (pred lastLayer)
            ]
        ]
    | i <- channels
    , i <= lastChannel - 2
    ]

psi_2 :: [[Lit (NetworkSynthesis 'Standard)]]
psi_2 = psi_2a ++ psi_2b ++ psi_2c

psi_3a :: [[Lit (NetworkSynthesis 'Standard)]]
psi_3a =
    [ 
        [ -gateLit i (i+1) lastLayer
        , -unusedLit (i+2) lastLayer
        , -unusedLit i     (pred lastLayer)
        , -unusedLit (i+1) (pred lastLayer)
        ]
    | i <- channels
    , i <= lastChannel - 2
    ]

psi_3b :: [[Lit (NetworkSynthesis 'Standard)]]
psi_3b =
    [ 
        [ -gateLit i (i+1) lastLayer
        , -unusedLit (i-1) lastLayer
        , -unusedLit i     (pred lastLayer)
        , -unusedLit (i+1) (pred lastLayer)
        ]
    | i <- succeeding firstChannel
    , i <= lastChannel - 1
    ]

psi_3 :: [[Lit (NetworkSynthesis 'Standard)]]
psi_3 = psi_3a ++ psi_3b

lastConstr :: [[Lit (NetworkSynthesis 'Standard)]]
lastConstr = concat
    [ psi_1
    , psi_2
    , psi_3
    , phi_1
    , phi_2
    , phi_3
    , phi_4
    ]