{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module MinDepthSN.SAT.Synthesis.ConstraintsBZ where

import Prelude hiding (negate)
import Data.Word (Word32)
import Data.Ix
import Data.Enum (succeeding, between)
import SAT.IPASIR (Lit(..), negate, polarize)
-- import MinDepthSN.SAT.Synthesis.VarsBZ (NetworkSynthesis(Value_))
import MinDepthSN.SAT.Constraints 
    ( fixGateOrUnused
    , iffDisjunctionOf
    , litImplies
    , exactlyOneOf
    , noneOf
    )
import MinDepthSN.Vars
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
    usagesOfChan i = map (PosLit . gateOrUnused_) . filter (usesChannel i)
    gatesInLayer :: Layer -> [GateOrUnused t]
    gatesInLayer k = range ( GateOrUnused minBound minBound k  -- can be improved
                           , GateOrUnused maxBound maxBound k)
    usesChannel :: Channel -> GateOrUnused t -> Bool
    usesChannel l (GateOrUnused i j _k) = i == l || j == l

-- gates between adjacent channels and ranges up to a single channel are already
-- mapped to the same DIMACS value. Thus they are skipped here
usageOfOneInUpTo :: [[Lit (NetworkSynthesis 'Standard)]]
usageOfOneInUpTo = concat $
    [ PosLit (toOneInUpTo_ t) `iffDisjunctionOf` [gateLit l j k | l <- range (i, pred j)]
    | t@(ToOneInUpTo i j k) <- [ minBound .. maxBound ] 
    , not $ areAdjacent i j
    ] ++
    [ PosLit (fromOneInUpTo_ f) `iffDisjunctionOf` [gateLit i l k | l <- range (succ i, j)]
    | f@(FromOneInUpTo i j k) <- [ minBound .. maxBound ]
    , not $ areAdjacent i j
    ]


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
update cexOffset = concat
    [-- TODO: replace (map . map . fmap) by fmap for a CNF datatype like: data CNF a = CNF [[Lit a]] deriving Functor
       PosLit (gateOrUnused_ gu) `litImplies` (map . map . fmap) (\v -> v cexOffset) (fixGateOrUnused gu)
    | gu <- [ minBound .. maxBound ] :: [GateOrUnused t]
    ]

updateEM :: Word32 -> (Channel, Channel) -> [[Lit (NetworkSynthesis 'Standard)]]
updateEM cexOffset (l, u) = concat
    [ concat
        [ PosLit (gate_ g) `litImplies` fixGate g
        | g <- [ minBound .. maxBound ]
        ]
    , 
        [ [ toOneInUpToLit l j k, inp j k, -outp j k ] -- not toOneInUpToLit implies j is min channel or unused
        | j <- range (succ l, u)
        , k <- layers
        ]
    , 
        [ [ fromOneInUpToLit i u k, -inp i k, outp i k ] -- not fromOneInUpToLit implies i is max channel or unused
        | i <- range (l, pred u)
        , k <- layers
        ]
    ,   [ [ inp l k, -outp l k ] -- l (first channel) is min channel or unused
        | k <- layers
        ]
    ,   [ [ -inp u k, outp u k ] -- u (last channel) is max channel or unused
        | k <- layers
        ]
    ]
  where
    fixGate :: KnownNetType t => Gate t -> [[Lit (NetworkSynthesis t)]]
    fixGate g = case g of
        Gate i j k -> 
            [ [ outp i k, -inp j k, -inp i k ]
            , [-outp i k, inp j k ]
            , [-outp j k, inp j k, inp i k ]
            , [ outp j k,-inp i k ]
            ]
    inp, outp :: Channel -> Layer -> Lit (NetworkSynthesis t)
    inp  i k = PosLit (Value i (before k) cexOffset)
    outp i k = PosLit (Value i (after  k) cexOffset)

-- improved update constraints that enable more propagations by Thorsten Ehlers and Mike Müller
sortsEM :: Word32 -> [Bool] -> [[Lit (NetworkSynthesis 'Standard)]]
sortsEM cexOffset counterexample = concat
    [ zipWith fixValue counterexample $ map value_ $ range (Value l beforeFirstLayer, Value u beforeFirstLayer)
    , updateEM cexOffset (l, u)
    , zipWith fixValue (sort counterexample) $ map value_ $ range (Value l afterLastLayer, Value u afterLastLayer)
    ]
  where
    l, u :: Channel
    (l, u) = (firstChannel, lastChannel)
    fixValue :: Bool -> (Word32 -> NetworkSynthesis t) -> [Lit (NetworkSynthesis t)]
    fixValue polarity mkVal = [polarize polarity (mkVal cexOffset)]

sorts :: forall t. KnownNetType t => Word32 -> [Bool] -> [[Lit (NetworkSynthesis t)]]
sorts cexOffset counterexample = concat
    [ zipWith fixValue counterexample inputValues
    , update cexOffset
    , zipWith fixValue (sort counterexample) outputValues
    ]
  where
    fixValue :: Bool -> (Word32 -> NetworkSynthesis t) -> [Lit (NetworkSynthesis t)]
    fixValue polarity mkVal = [polarize polarity (mkVal cexOffset)]

-- for generalized networks
-- behavior on unused not implemented
-- not (yet?) correct!!!
representativesOfBzIsomorphicEqClasses :: [[Lit (NetworkSynthesis 'Generalized)]]
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


