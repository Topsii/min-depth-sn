{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MinDepthSN.SAT.Synthesis.ConstraintsBZ where

import Prelude hiding (negate)
import SAT.IPASIR.EnumVarSolver (Var(..), Lit, negate, polarize, lit)
import MinDepthSN.SAT.Synthesis.VarsBZ (NetworkSynthesis(Value_), StandardNetworkSynthesis, GeneralizedNetworkSynthesis)
import Numeric.Natural (Natural)
import Enumerate (enumerated)
import MinDepthSN.SAT.Constraints 
    ( fixGateOrUnused
    , litImplies
    , exactlyOneOf
    , noneOf
    )
import MinDepthSN.Data.Value (Value, inputValues, outputValues)
import MinDepthSN.Data.GateOrUnused (GateOrUnused(..), gateOrUnusedLit)
import MinDepthSN.Data.Unused (unusedLit)
import MinDepthSN.Data.Gate (SortOrder, SortingOrder(..), sortOrder, gateLit)
import MinDepthSN.Data.Size (Layer, channels, succeeding, between, layers, n)
import Data.List (sort)


-- | Each channel \(i\) is either compared with some channel \(j\) or not 
-- used in layer \(k\).
--
-- \[
-- \bigwedge_{0 \le i < n} \ 
-- \bigwedge_{0 \le k < d} \ 
-- \bigvee_{0 \le j < n} 
--     gu_{i,j}^k
-- \]
--
-- Each channel \(i\) is also used by at most one comparator in each layer \(k\).
--
-- \[
-- \bigwedge_{0 \le i < n} \ 
-- \bigwedge_{0 \le k < d} \ 
-- \bigwedge_{0 \le j < l < n} 
--     \left( \neg gu_{i,j}^k \vee \neg gu_{i,l}^k \right)
-- \]
--
-- Recall that \(gu_{i,i}^k\) stands for channel \(i\) not being used. Thus
-- \(i\) cannot be unused and at the same time be compared with some 
-- channel \(l\) or \(j\).
--
-- Further note that \(gu_{i,j}^k\) and \(gu_{j,i}^k\) refer to the same
-- variable.
--
-- For the definition of \(gu\) see 'MinDepthSN.Data.GateOrUnused.GateOrUnused'.
--
-- Notably there are additional two-literal-clauses associating \(g_{j,i}^k\) 
-- with \(unused_i^k\). Such clauses were not contained in previous encodings.
--
usage :: forall o. SortOrder o => [[Lit (NetworkSynthesis o)]]
usage = concatMap exactlyOneOf
    [
        concat 
        [ if sortOrder (GateOrUnused i j k :: GateOrUnused o) == Standard
            then [ lit $ (GateOrUnused (min i j) (max i j) k :: GateOrUnused o) ]
            else [ gateOrUnusedLit i j k, gateOrUnusedLit j i k ]
        | j <- channels
        ]
    | i <- channels
    , k <- layers
    ]

ifThenElse :: Bool -> a -> a -> a
ifThenElse True t _ = t
ifThenElse False _ f = f

-- | The first layer is maximal (cf. Bundala and Zavodny "A layer \(L\) is 
-- called maximal if no more comparators can be added into \(L\)."). Hence there
-- is at most one unused channel in the first layer. More precisely:
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
maximalFirstLayer :: forall o. SortOrder o => [[Lit (NetworkSynthesis o)]]
maximalFirstLayer
    | even n = noneOf unusedLitsInFirstLayer
    | otherwise = exactlyOneOf unusedLitsInFirstLayer
  where
    unusedLitsInFirstLayer :: [Lit (NetworkSynthesis o)]
    unusedLitsInFirstLayer = [ unusedLit i 0 | i <- channels ]

-- | Values of a given counterexample input are propagated along the channels 
-- according to the placement of the comparators.
--
-- For the definition of \(gu\) see 'MinDepthSN.Data.GateOrUnused.GateOrUnused'.
--
-- \[
-- \bigwedge_{0 \le i \le j < n} \ 
-- \bigwedge_{0 \le k < d}
--     gu_{i,j}^k \rightarrow \left( \\
--         \left( v_i^{k+1} \leftrightarrow 
--             \left( v_i^k \wedge v_j^k \right) \right)
--         \wedge \\
--         \left( v_j^{k+1} \leftrightarrow 
--             \left( v_i^k \vee v_j^k \right) \right) \right)\\
-- \]
--
-- See 'MinDepthSN.SAT.Constraints.litImplies' and
-- 'MinDepthSN.SAT.Constraints.fixGateOrUnused' for the CNF.
--
update :: forall o. SortOrder o => Natural -> [[Lit (NetworkSynthesis o)]]
update cexIdx = concat
    [-- TODO: replace (map . map . fmap) by fmap for a CNF datatype like: data CNF a = CNF [[Lit a]] deriving Functor
        gateOrUnusedLit i j k `litImplies` (map . map . fmap) (Value_ cexIdx) (fixGateOrUnused (GateOrUnused i j k :: GateOrUnused o))
    | GateOrUnused i j k <- enumerated :: [GateOrUnused o]
    ]

sorts :: SortOrder o => Natural -> [Bool] -> [[Lit (NetworkSynthesis o)]]
sorts cexIdx counterexample = concat
    [ zipWith fixValue counterexample inputValues
    , update cexIdx
    , zipWith fixValue (sort counterexample) outputValues
    ]
  where
    fixValue :: Bool -> Value -> [Lit (NetworkSynthesis o)]
    fixValue polarity val = [polarize polarity . Var $ Value_ cexIdx val]

-- bad behavior on unused
representativesOfBzIsomorphicEqClasses :: [[Lit GeneralizedNetworkSynthesis]]
representativesOfBzIsomorphicEqClasses = concat
    [ 
        let predK = pred k 
        in 
            [ [ -gateLit i j k, -gateLit h i predK, -gateLit j l predK ]
            , [ -gateLit i j k, -gateLit h i predK, -gateLit l j predK ]
            , [ -gateLit i j k, -gateLit i h predK, -gateLit j l predK ]
            , [ -gateLit i j k, -gateLit i h predK, -gateLit l j predK ]
            ]
    | h <- channels
    , i <- channels
    , j <- channels
    , l <- channels
    , k <- drop 1 layers
    , i /= j
    , i /= h
    , i /= l
    , j /= h
    , j /= l
    , h < l -- TODO
    ]

outsideSpan :: [[Lit StandardNetworkSynthesis]]
outsideSpan = concat
    [
        gateLit i j k `litImplies` 
        [ [ -gateLit o j l ] -- use oneUp/oneDown instead of ranging over o/p?
        , [ -gateLit i p l ] 
        ]
-- should there be a variable like oneUp/oneDown, but additionally
-- spanning from (the first?) layer to layer x?
-- must be set for diff in and out value on same chan
    | i <- channels
    , j <- succeeding i
    , k <- layers
    , l <- [ minBound .. k ]
    , o <- i : between i j
    , p <- j : between i j
    , k /= l
    ]



maximal :: forall o. SortOrder o => [[Lit (NetworkSynthesis o)]]
maximal
    | even n    = concat [ noneOf       (unusedLitsInLayer k) | k <- layers ]
    | otherwise = concat [ exactlyOneOf (unusedLitsInLayer k) | k <- layers ]
  where
    unusedLitsInLayer :: Layer -> [Lit (NetworkSynthesis o)]
    unusedLitsInLayer k = [ unusedLit i k | i <- channels ]


