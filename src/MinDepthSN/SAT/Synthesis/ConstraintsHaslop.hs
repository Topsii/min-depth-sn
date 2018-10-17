{-# LANGUAGE RebindableSyntax #-}

module MinDepthSN.SAT.Synthesis.ConstraintsHaslop where

import Prelude hiding (negate)
import Data.Enum (preceding, succeeding)
import Enumerate.Enum.Valid (Validatable(..))
import Enumerate (enumerated)
import SAT.IPASIR (Lit(..), negate, Var(..))
import MinDepthSN.SAT.Synthesis.VarsHaslop
import MinDepthSN.Data.GateOut
import MinDepthSN.Data.GateIn
import MinDepthSN.Data.Size

-- Symmetry breaking on the gate indices (Breaking the layer indices follows 
-- from breaking the layer indices of the GateIn vars).
--
-- Given an order on GateOut ignoring the gate index:
-- For any two gate indices a and b after layer k, ensure that the assigned 
-- GateOut of i is smaller than that of j.
minimalRepresentative :: [[Lit Int]]
minimalRepresentative = (map ((:[]) . Positive . Var ) [1..x])
x :: Int
x = length (enumerated :: [GateOut])
    -- [
    --     -- instead (GateOut o p b h i k) and (GateOut r s a j l k) is a solution
    --     -- where we ensure a < b implies GateOut r s (a) j l k < GateOut o p (b) h i k
    --     -- the brackets indicate that a and b are irrelevant for ordering the GateOuts
    --     [ - gateOutLit o p a h i k, - gateOutLit r s b j l k ]
    -- | a <- gatesInLayer
    -- , b <- succeeding a
    -- , k <- layers
    -- , o <- gatesInLayer
    -- , p <- gatesInLayer
    -- , r <- gatesInLayer
    -- , s <- gatesInLayer
    -- , h <- succeeding k :: [Layer]
    -- , i <- succeeding k :: [Layer]
    -- , j <- succeeding k :: [Layer]
    -- , l <- succeeding k :: [Layer]
    -- -- if GateOut of a is greater than GateOut of b , we will add a preventing constraint, though a < b:
    -- , GateOut o p 0 h i k > GateOut r s 0 j l k
    -- ]


gateInFromGateOut :: [[Lit NetworkGraphSynthesis]]
gateInFromGateOut = concat
    [
        concat
        [ [ [ - gateOutLit a b i k l p, - gateOutLit a c j k h o, gateInLit iMin jMin a p o k ]
          | isValid $ UnvalidatedGateIn iMin jMin a p o k ]
        , [ [ - gateOutLit a b i k l p, - gateOutLit c a j h k o, gateInLit iMin jMax a p o k ]
          | isValid $ UnvalidatedGateIn iMin jMax a p o k ]
        , [ [ - gateOutLit b a i l k p, - gateOutLit a c j k h o, gateInLit iMax jMin a p o k ]
          | isValid $ UnvalidatedGateIn iMax jMin a p o k ]
        , [ [ - gateOutLit b a i l k p, - gateOutLit c a j h k o, gateInLit iMax jMax a p o k ]
          | isValid $ UnvalidatedGateIn iMax jMax a p o k ]
        ]
    | a <- gatesInLayer
    , b <- gatesInLayer
    , c <- gatesInLayer
    , k <- layers
    , i <- gatesInLayer
    , j <- gatesInLayer
    , let iMin = minForwardChannel i
    , let iMax = maxForwardChannel i
    , let jMin = minForwardChannel j
    , let jMax = maxForwardChannel j
    , o <- preceding k :: [Layer]
    , p <- preceding k :: [Layer]
    , h <- succeeding o :: [Layer]
    , l <- succeeding p :: [Layer]
    ]


-- gateOutFromGateIn :: [[Lit NetworkGraphSynthesis]]
-- gateOutFromGateIn = 
--     [
--         concat
--         [ [ [ - gateInLit iOut jMin a _ _ _, - gateInLit jMax mOut b _ _ _, gateOutLit a b j _ _ _ ]
--           | isValid $ UnvalidatedGateOut a b j _ _ _ ]
--         , [ [ - gateInLit iOut jMax a _ _ _, - gateInLit jMin mOut b _ _ _, gateOutLit b a j _ _ _ ]
--           | isValid $ UnvalidatedGateOut b a j _ _ _ ]
--         , [ - gateInLit iOut jMin a _ _ _, - gateInLit jMin mOut b _ _ _ ]
--         , [ - gateInLit iOut jMax a _ _ _, - gateInLit jMax mOut b _ _ _ ]
--         ]
--     | a <- gatesInLayer
--     , b <- gatesInLayer
--     , iOut <- channels
--     , j <- gatesInLayer
--     , mOut <- channels
--     , even n || iOut /= maxBound
--     , even n || mOut /= maxBound
--     , let jMin = minForwardChannel j
--     , let jMax = maxForwardChannel j
--     , k <- layers
--     , l <- succeeding p :: [Layer]
--     , h <- succeeding p :: [Layer]
--     ]