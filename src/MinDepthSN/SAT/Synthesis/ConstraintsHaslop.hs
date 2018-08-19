{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -w #-}
module MinDepthSN.SAT.Synthesis.ConstraintsHaslop where

import Prelude hiding (negate)
import SAT.IPASIR.EnumVars (Var(..), Lit(..), negate)
import MinDepthSN.Data.GateOut
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

minimalRepresentative :: (a -> a -> Bool) -> (a -> a -> Bool) -> [a] -> [[Lit a]]
minimalRepresentative isCongruent isSmaller _ = [  ]


