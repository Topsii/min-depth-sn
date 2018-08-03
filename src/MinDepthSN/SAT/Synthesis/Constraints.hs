{-# LANGUAGE RebindableSyntax #-}
{-# language PatternSynonyms #-}

module MinDepthSN.SAT.Synthesis.Constraints where

import Prelude hiding (negate)
import SAT.IPASIR.EnumVars (Var(..), Lit, negate, polarize)
import MinDepthSN.SAT.Synthesis.Variables (NetworkSynthesis(Value_), pattern ValueVar, pattern GateOrUnused, compOrUnusedLit, unusedLit, channels, channelsAfter, layers, afterLayers, n)
import Numeric.Natural
import MinDepthSN.SAT.Constraints (fixGateOrUnused, litImplies)
import MinDepthSN.Data.Value (Value, inputValues, outputValues)
import Data.List (sort)

-- | Each channel \(i\) is used by at most one comparator in each layer \(k\).
-- Two comparators in \(k\) comparing \(i\) with channels \(j\) and \(l\) 
-- respectively will contradict this proposition.
--
-- Also recall that \(gu_{i,i}^k\) stands for channel \(i\) not being used. Thus
-- \(i\) cannot be unused and at the same time be compared with some 
-- channel \(l\) or \(j\).
--
-- Further note that \(gu_{i,j}^k\) and \(gu_{j,i}^k\) refer to the same
-- variable.
--
-- For the definition of \(gu\) see 
-- 'MinDepthSN.SAT.Synthesis.Variables.GateOrUnusedVar'.
--
-- \[
-- \bigwedge_{0 \le i < n} \ 
-- \bigwedge_{0 \le k < d} \ 
-- \bigwedge_{0 \le j < l < n} 
--     \left( \neg gu_{i,j}^k \vee \neg gu_{i,l}^k \right)
-- \]
--
-- Notably there are additional two-literal-clauses associating \(g_{j,i}^k\) 
-- with \(unused_i^k\). Such clauses were not contained in previous encodings.
--
usageOnce :: [[Lit NetworkSynthesis]]
usageOnce =
    [ 
        [ -compOrUnusedLit i j k
        , -compOrUnusedLit i l k
        ]
    | i <- channels
    , j <- channels
    , l <- channelsAfter j
    , k <- layers
    ]

-- | Each channel \(i\) is either compared with some channel \(j\) or not 
-- used in layer \(k\).
--
-- Also recall that \(gu_{i,i}^k\) stands for channel \(i\) not being used. 
--
-- Further note that \(gu_{i,j}^k\) and \(gu_{j,i}^k\) refer to the same 
-- variable.
--
-- For the definition of \(gu\) see 
-- 'MinDepthSN.SAT.Synthesis.Variables.GateOrUnusedVar'.
--
-- \[
-- \bigwedge_{0 \le i < n} \ 
-- \bigwedge_{0 \le k < d} \ 
-- \bigvee_{0 \le j < n} 
--     gu_{i,j}^k
-- \]
--
unused :: [[Lit NetworkSynthesis]]
unused =
    [
        [ compOrUnusedLit i j k
        | j <- channels
        ]
    | i <- channels
    , k <- layers
    ]

-- | The first layer is maximal (cf. Bundala and Zavodny "A layer \(L\) is called
-- maximal if no more comparators can be added into \(L\)."). Hence there is at most
-- one unused channel in the first layer. More precisely:
--
-- There is no unused channel in a maximal layer iff. \(n\) is even.
--
-- There is exactly one unused channel in a maximal layer iff. \(n\) is odd.
--
-- \[
-- \begin{cases}
--     \bigwedge_{0 \le i < n} \  
--          \neg unused_i^0
--     & \text{if even}\ n \\
--     \bigwedge_{0 \le i < j < n} \
--         \left( \neg unused_i^0 \vee \neg unused_j^0 \right)
--     & \text{otherwise}
-- \end{cases}
-- \]
--
maximalFirstLayer :: [[Lit NetworkSynthesis]]
maximalFirstLayer
    | even n = 
        [
            [ -unusedLit i 0 ]
        | i <- channels
        ]
    | otherwise =
        [
            [ -unusedLit i 0, -unusedLit j 0 ]
        | i <- channels
        , j <- channelsAfter i
        ]

-- | Values of a given counterexample input are propagated along the channels 
-- according to the placement of the comparators.
--
-- For the definition of \(gu\) see 
-- 'MinDepthSN.SAT.Synthesis.Variables.GateOrUnusedVar'.
--
-- \[
-- \bigwedge_{0 \le i \le j < n} \ 
-- \bigwedge_{0 \le k < d}
--     gu_{i,j}^k \rightarrow \left( \\
--         \left( v_i^{k+1} \leftrightarrow \left( v_i^k \wedge v_j^k \right) \right)
--         \wedge \\
--         \left( v_j^{k+1} \leftrightarrow \left( v_i^k \vee v_j^k \right) \right) \right)\\
-- \]
--
-- See 'MinDepthSN.SAT.Constraints.litImplies' and
-- 'MinDepthSN.SAT.Constraints.fixGateOrUnused' for the CNF.
--
update :: Natural ->  [[Lit NetworkSynthesis]]
update cexIdx = concat
    [-- TODO: replace (map . map . fmap) by fmap for a CNF datatype like: data CNF a = CNF [[Lit a]] deriving Functor
        compOrUnusedLit i j k `litImplies` (map . map . fmap) (Value_ cexIdx) (fixGateOrUnused (GateOrUnused i j k))
    | i <- channels
    , j <- i : channelsAfter i
    , k <- layers
    ]

sorts :: Natural -> [Bool] -> [[Lit NetworkSynthesis]]
sorts cexIdx counterexample = concat
    [ zipWith fixValue counterexample inputValues
    , update cexIdx
    , zipWith fixValue (sort counterexample) outputValues
    ]
  where
    fixValue :: Bool -> Value -> [Lit NetworkSynthesis]
    fixValue polarity val = [polarize polarity . Var $ Value_ cexIdx val]










