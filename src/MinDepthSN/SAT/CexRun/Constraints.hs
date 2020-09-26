{-# LANGUAGE RebindableSyntax #-}

{-# LANGUAGE FlexibleContexts #-}

module MinDepthSN.SAT.CexRun.Constraints where

import Prelude hiding (negate)
import Data.List (inits, tails)
import Data.Enum (succeeding)
import SAT.IPASIR (assignments, solveCNFs, SolveResult(..), runSolver, Lit(..), negate)
import MinDepthSN.SAT.Constraints (fixGateOrUnused)
import MinDepthSN.Vars

-- find a counterexample run where some input is not sorted
findCounterexampleRun :: KnownNetType t => [GateOrUnused t] -> Maybe [Bool] -- unnecessary KnownNetType constraint?
findCounterexampleRun network = runSolver $ do
    s <- solveCNFs [fixNetwork network, unsortedOutput]
    case s of
        Unsatisfiable -> pure Nothing
        Satisfiable   -> do
        -- let positions = [ minBound .. maxBound ] :: [CexRun]
        -- vals <- assignments id positions
        -- let positionValues = zip vals positions
            counterexampleInput <- assignments value_ inputValues
            pure $ Just
                {-
                $ trace (
                    let sameLayer (GateOrUnused _ _ k) (GateOrUnused _ _ l) = k == l
                        sameLayer _ _ = error ""
                        sameLayer' (_,(Value _ k)) (_,(Value _ l)) = k == l
                        sameLayer' _ _ = error ""
                    in (unlines . map show $ groupBy sameLayer network) ++ "\n"
                    ++ (unlines . map show $ groupBy sameLayer' positionValues) ++ "\n")
                -}
                counterexampleInput


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
sortedOutput :: [[Lit CexRun]]
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
unsortedOutput :: [[Lit CexRun]]
unsortedOutput = zipWith (++) (inits outputOnes) (tails outputZeros)
  where
    outputZeros, outputOnes :: [Lit CexRun]
    outputZeros = map NegLit outputValues
    outputOnes  = map PosLit outputValues

-- | See 'MinDepthSN.SAT.Constraints.fixGateOrUnused'.
fixNetwork :: KnownNetType t => [GateOrUnused t] -> [[Lit CexRun]]
fixNetwork = concatMap fixGateOrUnused
