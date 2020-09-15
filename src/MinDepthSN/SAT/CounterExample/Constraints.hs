{-# LANGUAGE RebindableSyntax #-}

{-# LANGUAGE FlexibleContexts #-}

module MinDepthSN.SAT.Counterexample.Constraints where

import Prelude hiding (negate)
import Data.List (inits, tails)
import Data.Enum (succeeding)
import SAT.IPASIR (Var(..), Lit(..), negate)
import MinDepthSN.SAT.Constraints (fixGateOrUnused)
import MinDepthSN.SAT.Counterexample.Variables (Counterexample(..))
import MinDepthSN.Data.Size (channels, afterLastLayer)
import MinDepthSN.Data.Value (outputValues, valueLit)
import MinDepthSN.Data.GateOrUnused (GateOrUnused)
import MinDepthSN.Data.Gate (KnownNetType)
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
sortedOutput :: [[Lit Counterexample]]
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
unsortedOutput :: [[Lit Counterexample]]
unsortedOutput = zipWith (++) (inits outputOnes) (tails outputZeros)
  where
    outputZeros, outputOnes ::  [Lit Counterexample]
    outputZeros = map (Negative . Var . Counterexample) outputValues
    outputOnes  = map (Positive . Var . Counterexample) outputValues

-- | See 'MinDepthSN.SAT.Constraints.fixGateOrUnused'.
fixNetwork :: KnownNetType t => [GateOrUnused t] -> [[Lit Counterexample]]
-- TODO: replace (map . map . fmap) by fmap for a CNF datatype like: data CNF a = CNF [[Lit a]] deriving Functor
fixNetwork = concatMap ((map . map . fmap) Counterexample . fixGateOrUnused)
