{-# LANGUAGE RebindableSyntax #-}

{-# OPTIONS_GHC -w #-}
module MinDepthSN.SAT.Synthesis.ConstraintsHaslop where

import Prelude hiding (negate)
import SAT.IPASIR.EnumVars (Var(..), Lit(..), negate)
import MinDepthSN.Data.GateOut
import MinDepthSN.Data.Size
import Enumerate (enumerated)
import Data.List (inits)
import Data.Ord (comparing)
import Data.Monoid ((<>))

canonicalGraph :: [[Lit GateOut]]
canonicalGraph = 
    [
        [ -l1, -l2 ]
    | (litsBeforeL2, l2) <- zip (inits gateOuts) gateOuts
    , l1 <- litsBeforeL2
    ]
  where
    gateOuts :: [Lit GateOut]
    gateOuts = map (Pos . Var) enumerated

minimalRepresentative' :: (a -> a -> Bool) -> (a -> a -> Bool) -> [a] -> [[Lit a]]
minimalRepresentative' isCongruent isSmaller _ = [  ]


-- Symmetry breaking on the gate indices (Breaking the layer indices follows 
-- from breaking the layer indices of the GateIn vars).
--
-- Given an order on GateOut ignoring the gate index:
-- For any two gate indices a and b after layer k, ensure that the assigned 
-- GateOut of i is smaller than that of j.
minimalRepresentative :: [[Lit GateOut]]
minimalRepresentative = 
    [
        -- instead (GateOut o p b h i k) and (GateOut r s a j l k) is a solution
        -- where we ensure a < b implies GateOut r s (a) j l k < GateOut o p (b) h i k
        -- the brackets indicate that a and b are irrelevant for ordering the GateOuts
        [ Neg $ Var (GateOut o p a h i k), Neg $ Var (GateOut r s b j l k) ]
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
--             let giMinMin = Pos $ GateIn (minForwardChannel i) (minForwardChannel j) a p o k
--                 giMinMax = Pos $ GateIn (minForwardChannel i) (maxForwardChannel j) a p o k
--                 giMaxMin = Pos $ GateIn (maxForwardChannel i) (minForwardChannel j) a p o k
--                 giMaxMax = Pos $ GateIn (maxForwardChannel i) (maxForwardChannel j) a p o k
--             in
--             [ [Neg . Var $ GateOut a b i k l p, Neg . Var $ GateOut a c j k h o  -- Pos $ GateIn (minForwardChannel i) (minForwardChannel j) a p o k
--             | True --isValid (UnvalidatedGateIn (minForwardChannel i) (minForwardChannel j) a p o k)
--             ]

--         , [ Neg . Var $ GateOut a b i k l p, Neg . Var $ GateOut c a j h k o ]-- Pos $ GateIn (minForwardChannel i) (maxForwardChannel j) a p o k
--         , [ Neg . Var $ GateOut b a i l k p, Neg . Var $ GateOut a c j k h o ]-- Pos $ GateIn (maxForwardChannel i) (minForwardChannel j) a p o k
--         , [ Neg . Var $ GateOut b a i l k p, Neg . Var $ GateOut c a j h k o ]-- Pos $ GateIn (maxForwardChannel i) (maxForwardChannel j) a p o k
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