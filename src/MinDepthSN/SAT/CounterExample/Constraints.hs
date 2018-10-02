{-# LANGUAGE RebindableSyntax #-}

module MinDepthSN.SAT.CounterExample.Constraints where

import Prelude hiding (negate)
import Data.List (inits, tails)
import SAT.IPASIR (Var(..), Lit(..), negate)
import MinDepthSN.SAT.Constraints (fixGateOrUnused)
import MinDepthSN.SAT.CounterExample.Variables (CounterExample(..), valueLit)
import MinDepthSN.Data.Size (channels, succeeding, afterLastLayer)
import MinDepthSN.Data.Value (outputValues)
import MinDepthSN.Data.GateOrUnused (GateOrUnused, SortOrder)
--findCounterEx :: [Gate]
--findCounterEx = undefined


-- | The output of the network is sorted.
--
-- A 1 value in the output of the network on some channel \(i\) implies a 1 output 
-- on all higher indexed channels \(j\).
--
-- Likewise a 0 value in the output of the network on some channel \(j\) implies a 0 output 
-- on all lower indexed channels \(i\).
--
-- \[
-- \bigwedge_{0 \le i < j < n}
--      \left( \neg v_i^d \vee v_j^d \right)
-- \]
sortedOutput :: [[Lit CounterExample]]
sortedOutput =
    [
        [ - valueLit i afterLastLayer, valueLit j afterLastLayer ]
    | i <- channels
    , j <- succeeding i
    ]

-- | The output of the network is not sorted.
--
-- Excludes for e.g. \(n=3\) the output vectors: 000, 001, 011, 111.
-- To exclude a vector its negated literals are combined to a CNF clause.
--
-- \[
-- \bigwedge_{0 \le c \le n} 
--     \left( 
--         \bigvee_{0 \le i < c}  
--             v_i^d 
--         \vee 
--         \bigvee_{c \le j < n}
--             \neg v_j^d
--     \right)
-- \]
unsortedOutput :: [[Lit CounterExample]]
unsortedOutput = zipWith (++) (inits outputOnes) (tails outputZeros)
  where
    outputZeros ::  [Lit CounterExample]
    outputZeros = map (Negative . Var . CounterExample) outputValues
    outputOnes :: [Lit CounterExample]
    outputOnes = map (Positive . Var . CounterExample) outputValues

-- | See 'MinDepthSN.SAT.Constraints.fixGateOrUnused'.
fixNetwork :: SortOrder o => [GateOrUnused o] -> [[Lit CounterExample]]
-- TODO: replace (map . map . fmap) by fmap for a CNF datatype like: data CNF a = CNF [[Lit a]] deriving Functor
fixNetwork = concatMap ((map . map . fmap) CounterExample . fixGateOrUnused)
