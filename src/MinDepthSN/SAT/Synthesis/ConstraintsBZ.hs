{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE FlexibleContexts #-}

module MinDepthSN.SAT.Synthesis.ConstraintsBZ where

import Prelude hiding (negate)
import Data.Word (Word32)
import Data.Ix
import Data.Enum (succeeding, between)
import SAT.IPASIR (Lit, negate, polarize, lit)
import MinDepthSN.SAT.Synthesis.VarsBZ (NetworkSynthesis(Value_))
import MinDepthSN.SAT.Constraints 
    ( fixGateOrUnused
    , litImplies
    , exactlyOneOf
    , noneOf
    )
import MinDepthSN.Data.Value (Value, inputValues, outputValues)
import MinDepthSN.Data.GateOrUnused (GateOrUnused(..), gateOrUnusedLit)
import MinDepthSN.Data.Unused (unusedLit)
import MinDepthSN.Data.Gate (gateLit, KnownNetType)
import MinDepthSN.Data.Size (Layer, Channel, channels, layers, n)
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
usage :: forall t. KnownNetType t => [[Lit (NetworkSynthesis t)]]
usage = concat
    [ exactlyOneOf . usagesOfChan i $ gatesInLayer k
    | k <- layers 
    , i <- channels
    ]
  where
    usagesOfChan :: Channel -> [GateOrUnused t] -> [Lit (NetworkSynthesis t)]
    usagesOfChan i = map lit . filter (usesChannel i)
    gatesInLayer :: Layer -> [GateOrUnused t]
    gatesInLayer k = range ( GateOrUnused minBound minBound k  -- can be improved
                           , GateOrUnused maxBound maxBound k)
    usesChannel :: Channel -> GateOrUnused t -> Bool
    usesChannel l (GateOrUnused i j _k) = i == l || j == l

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
maximalFirstLayer :: forall t. KnownNetType t => [[Lit (NetworkSynthesis t)]]
maximalFirstLayer
    | even n = noneOf unusedLitsInFirstLayer
    | otherwise = exactlyOneOf unusedLitsInFirstLayer
  where
    unusedLitsInFirstLayer :: [Lit (NetworkSynthesis t)]
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
update :: forall t. KnownNetType t => Word32 -> [[Lit (NetworkSynthesis t)]]
update cexIdx = concat
    [-- TODO: replace (map . map . fmap) by fmap for a CNF datatype like: data CNF a = CNF [[Lit a]] deriving Functor
        gateOrUnusedLit i j k `litImplies` (map . map . fmap) (Value_ cexIdx) (fixGateOrUnused (GateOrUnused i j k :: GateOrUnused t))
    | GateOrUnused i j k <- [ minBound .. maxBound ] :: [GateOrUnused t]
    ]

-- -- improved update constraints that enable more propagations by Thorsten Ehlers and Mike Müller
-- sortsEM :: forall t. KnownNetType t => Word32 -> [Bool] -> [[Lit (NetworkSynthesis t)]]
-- sortsEM cexIdx counterexample = concat
--     [ zipWith fixValue counterexample inputValues
--     , updateEM (firstChannel, lastChannel) cexIdx
--     , zipWith fixValue (sort counterexample) outputValues
--     ]
--   where
--     fixValue :: Bool -> Value -> [Lit (NetworkSynthesis t)]
--     fixValue polarity val = [polarize polarity (cexIdx, val)]

-- updateEM :: forall t. KnownNetType t => Word32 -> (Channel, Channel) -> [[Lit (NetworkSynthesis)]]
-- updateEM cexIdx (l,h) = concat
--     [ 
--     | Gate i j k
--     ]
--   where
--     fixGateOrUnused :: KnownNetType t => GateOrUnused t -> [[Lit Value]]
--     fixGateOrUnused (GateOrUnused i j k) =
--         minimum [in1, in2] outMin ++ maximum [in1, in2] outMax
--     where
--         beforeK, afterK :: BetweenLayers
--         beforeK = before k
--         afterK = after k
--         in1, in2, outMin, outMax :: Lit Value
--         in1 = valueLit i beforeK
--         in2 = valueLit j beforeK
--         outMin = valueLit i afterK
--         outMax = valueLit j afterK

sorts :: forall t. KnownNetType t => Word32 -> [Bool] -> [[Lit (NetworkSynthesis t)]]
sorts cexIdx counterexample = concat
    [ zipWith fixValue counterexample inputValues
    , update cexIdx
    , zipWith fixValue (sort counterexample) outputValues
    ]
  where
    fixValue :: Bool -> Value -> [Lit (NetworkSynthesis t)]
    fixValue polarity val = [polarize polarity (cexIdx, val)]

-- for generalized networks
-- behavior on unused not implemented
-- not (yet?) correct!!!
representativesOfBzIsomorphicEqClasses :: KnownNetType t => [[Lit (NetworkSynthesis t)]]
representativesOfBzIsomorphicEqClasses = concat
    [ 
        let predK = pred k 
        in 
            [ [ - gateLit i j k, - gateLit h i predK, - gateLit j l predK ]
            , [ - gateLit i j k, - gateLit h i predK, - gateLit l j predK ]
            , [ - gateLit i j k, - gateLit i h predK, - gateLit j l predK ]
            , [ - gateLit i j k, - gateLit i h predK, - gateLit l j predK ]
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

-- for standard networks
outsideSpan :: KnownNetType t => [[Lit (NetworkSynthesis t)]]
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



maximal :: forall t. KnownNetType t => [[Lit (NetworkSynthesis t)]]
maximal
    | even n    = concat [ noneOf       (unusedLitsInLayer k) | k <- layers ]
    | otherwise = concat [ exactlyOneOf (unusedLitsInLayer k) | k <- layers ]
  where
    unusedLitsInLayer :: Layer -> [Lit (NetworkSynthesis t)]
    unusedLitsInLayer k = [ unusedLit i k | i <- channels ]


