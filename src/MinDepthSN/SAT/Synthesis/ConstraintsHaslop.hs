{-# LANGUAGE RebindableSyntax #-}

{-# OPTIONS_GHC -w #-}
module MinDepthSN.SAT.Synthesis.ConstraintsHaslop where

import Prelude hiding (negate)
import Data.List (inits)
import Data.Ord (comparing)
import Data.Monoid ((<>))
import Enumerate (enumerated)
import SAT.IPASIR (Var(..), Lit(..), negate, lit)
import MinDepthSN.SAT.Synthesis.VarsHaslop
import MinDepthSN.Data.GateOut
import MinDepthSN.Data.Size

canonicalGraph :: [[Lit GateOut]]
canonicalGraph = 
    [
        [ -l1, -l2 ]
    | (litsBeforeL2, l2) <- zip (inits gateOuts) gateOuts
    , l1 <- litsBeforeL2
    ]
  where
    gateOuts :: [Lit GateOut]
    gateOuts = map (Positive . Var) enumerated

minimalRepresentative' :: (a -> a -> Bool) -> (a -> a -> Bool) -> [a] -> [[Lit a]]
minimalRepresentative' isCongruent isSmaller _ = [  ]


-- Symmetry breaking on the gate indices (Breaking the layer indices follows 
-- from breaking the layer indices of the GateIn vars).
--
-- Given an order on GateOut ignoring the gate index:
-- For any two gate indices a and b after layer k, ensure that the assigned 
-- GateOut of i is smaller than that of j.
minimalRepresentative :: [[Lit NetworkGraphSynthesis]]
minimalRepresentative = 
    [
        -- instead (GateOut o p b h i k) and (GateOut r s a j l k) is a solution
        -- where we ensure a < b implies GateOut r s (a) j l k < GateOut o p (b) h i k
        -- the brackets indicate that a and b are irrelevant for ordering the GateOuts
        [ - lit (GateOut o p a h i k), - lit (GateOut r s b j l k) ]
    | a <- gatesInLayer
    , b <- succeeding a
    , k <- layers
    , o <- gatesInLayer
    , p <- gatesInLayer
    , r <- gatesInLayer
    , s <- gatesInLayer
    , h <- succeeding k :: [Layer]
    , i <- succeeding k :: [Layer]
    , j <- succeeding k :: [Layer]
    , l <- succeeding k :: [Layer]
    -- if GateOut of a is greater than GateOut of b , we will add a preventing constraint, though a < b:
    , GateOut o p 0 h i k > GateOut r s 0 j l k
    ]


-- gateInFromGateOut :: [[Lit GateOut]]
-- gateInFromGateOut = concat
--     [
--         [ 
--             let giMinMin = Positive $ GateIn (minForwardChannel i) (minForwardChannel j) a p o k
--                 giMinMax = Positive $ GateIn (minForwardChannel i) (maxForwardChannel j) a p o k
--                 giMaxMin = Positive $ GateIn (maxForwardChannel i) (minForwardChannel j) a p o k
--                 giMaxMax = Positive $ GateIn (maxForwardChannel i) (maxForwardChannel j) a p o k
--             in
--             [ [Negative . Var $ GateOut a b i k l p, Negative . Var $ GateOut a c j k h o  -- Positive $ GateIn (minForwardChannel i) (minForwardChannel j) a p o k
--             | True --isValid (UnvalidatedGateIn (minForwardChannel i) (minForwardChannel j) a p o k)
--             ]

--         , [ Negative . Var $ GateOut a b i k l p, Negative . Var $ GateOut c a j h k o ]-- Positive $ GateIn (minForwardChannel i) (maxForwardChannel j) a p o k
--         , [ Negative . Var $ GateOut b a i l k p, Negative . Var $ GateOut a c j k h o ]-- Positive $ GateIn (maxForwardChannel i) (minForwardChannel j) a p o k
--         , [ Negative . Var $ GateOut b a i l k p, Negative . Var $ GateOut c a j h k o ]-- Positive $ GateIn (maxForwardChannel i) (maxForwardChannel j) a p o k
--         ]
--     | a <- gatesInLayer
--     , b <- gatesInLayer
--     , c <- gatesInLayer
--     , k <- layers
--     , i <- gatesInLayer
--     , j <- gatesInLayer
--     , o <- preceding k :: [Layer]
--     , p <- preceding k :: [Layer]
--     , h <- succeeding o :: [Layer]
--     , l <- succeeding p :: [Layer]
--     ]


gateOutFromGateIn :: [[Lit a]]
gateOutFromGateIn = undefined