{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module MinDepthSN.SAT.Synthesis.ConstraintsBZ where

import Prelude hiding (negate)
import Data.Word (Word32)
import Data.Ix
import Data.Enum (succeeding, between)
import Data.Pair
import SAT.IPASIR (Lit(..), negate, polarize)
import MinDepthSN.SAT.Constraints 
    ( fixGateOrUnused
    , iffDisjunctionOf
    , litImplies
    , exactlyOneOf
    , noneOf
    , atMostOneOf 
    )
import MinDepthSN.Data.Window ( windowBounds )
import MinDepthSN.Vars
import Data.List (genericDrop, sort)
import Data.Maybe (catMaybes)

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
defMinMaxViaGates :: [[Lit (NetworkSynthesis 'Standard)]]
defMinMaxViaGates = concat $
    [ PosLit (max_ t) `iffDisjunctionOf` [gateLit l j k | l <- range (i, pred j)]
    | t@(Max i j k) <- [ minBound .. maxBound ] 
    , not $ areAdjacent i j
    ] ++
    [ PosLit (min_ f) `iffDisjunctionOf` [gateLit i l k | l <- range (succ i, j)]
    | f@(Min i j k) <- [ minBound .. maxBound ]
    , not $ areAdjacent i j
    ]

defMinMaxViaGatesRec :: [[Lit (NetworkSynthesis 'Standard)]]
defMinMaxViaGatesRec = concat $
    [ PosLit (max_ t) `iffDisjunctionOf` [gateLit i j k, maxLit (succ i) j k]
    | t@(Max i j k) <- [ minBound .. maxBound ] 
    , not $ areAdjacent i j
    ] ++
    [ PosLit (min_ f) `iffDisjunctionOf` [gateLit i j k, minLit i (pred j) k]
    | f@(Min i j k) <- [ minBound .. maxBound ]
    , not $ areAdjacent i j
    ]

amoMinMaxRec :: [[Lit (NetworkSynthesis 'Standard)]]
amoMinMaxRec = concat $
    [ atMostOneOf [gateLit i j k, maxLit (succ i) j k]
    | Max i j k <- [ minBound .. maxBound :: Max 'Standard] 
    , not $ areAdjacent i j
    ] ++
    [ atMostOneOf [gateLit i j k, minLit i (pred j) k]
    | Min i j k <- [ minBound .. maxBound :: Min 'Standard]
    , not $ areAdjacent i j
    ]
    ++
    [ exactlyOneOf $ catMaybes
        [ Just $ unusedLit i k
        , maxLit' firstChannel i k
        , minLit' i lastChannel k
        ]
    | k <- layers
    , i <- channels
    ]
    {- ++
    [ atMostOneOf $ catMaybes
        [ maxLit' firstChannel i k
        , minLit' i lastChannel k
        ]
    | k <- layers
    , i <- channels
    ]-}

-- alternative to the usage constraint with smaller clauses
amoMinMax :: [[Lit (NetworkSynthesis 'Standard)]]
amoMinMax = concat $
    [ atMostOneOf [gateLit l j k | l <- range (i, pred j)]
    | Max i j k <- [ minBound .. maxBound :: Max 'Standard] 
    ] ++
    [ atMostOneOf [gateLit i l k | l <- range (succ i, j)]
    | Min i j k <- [ minBound .. maxBound :: Min 'Standard]
    ]
    ++
    [ (atMostOneOf $ catMaybes
        [ Just $ unusedLit i k
        , maxLit' firstChannel i k
        -- , minLit' i lastChannel k
        ]) ++ (atMostOneOf $ catMaybes
        [ Just $ unusedLit i k
        -- , maxLit' firstChannel i k
        , minLit' i lastChannel k
        ])
    | k <- layers
    , i <- channels
    ]
    {- ++
    [ atMostOneOf $ catMaybes
        [ maxLit' firstChannel i k
        , minLit' i lastChannel k
        ]
    | k <- layers
    , i <- channels
    ]-}
        

toBetweenBeforeConstr :: [[Lit (NetworkSynthesis 'Standard)]]
toBetweenBeforeConstr = concat
    [ defineToBetweenBefore tbb | tbb <- [ minBound .. maxBound ] ]
  where
    defineToBetweenBefore :: ToBetweenBefore -> [[Lit (NetworkSynthesis 'Standard)]]
    defineToBetweenBefore tbb@(ToBetweenBefore i j k) = case k of
        0 -> [[ NegLit $ toBetweenBefore_ tbb ]] 
        _ -> PosLit (toBetweenBefore_ tbb) `iffDisjunctionOf`
                [ toBetweenBeforeLit i j (pred k)
                , minLit i j k
                , maxLit i j k
                ]

viaWrongTwistConstr :: [[Lit (NetworkSynthesis 'Standard)]]
viaWrongTwistConstr = concat $
    [ defineViaWrongTwist vwt | vwt <- [ minBound .. maxBound ] ]
  where
    defineViaWrongTwist :: ViaWrongTwist -> [[Lit (NetworkSynthesis 'Standard)]]
    defineViaWrongTwist vwt@(ViaWrongTwist p o k) = case k of
        0 -> [[ NegLit $ viaWrongTwist_ vwt ]] -- should (hopefully!) be irrelevant
        _ -> [
                [ PosLit (viaWrongTwist_ vwt)
                , toBetweenBeforeLit i j k
                , -gateLit (min i o) (max i o) (pred k)
                , -gateLit (min j p) (max j p) (pred k)
                , NegLit (gate_ g)
                ]
            | g@(Gate i j _k) <- range ( Gate minBound maxBound k, Gate minBound maxBound k)
            , i /= o
            , i /= p
            , j /= o
            , j /= p
            ]

data ParGatesLayer
    = Earliest -- ^ Move every gate to the earliest layer possible
    | Latest   -- ^ Move every gate to the latest layer possible
    
breakParallelGates :: ParGatesLayer -> [[Lit (NetworkSynthesis 'Standard)]]
breakParallelGates pgl = 
    [ 
        [ -unusedLit i uk
        , -unusedLit j uk
        , - gateLit i j gk
        ]
    | k <- succeeding (0 :: Layer)   
    , let (uk, gk) = case pgl of
            Earliest -> (pred k, k)
            Latest   -> (k, pred k)
    , i <- channels
    , j <- succeeding i
    ]

breakInpTwists :: [[Lit (NetworkSynthesis 'Standard)]]
breakInpTwists =
    concat
        [ breakInpTwist g | g <- [ minBound .. maxBound ]] 
    ++
    concat
        [ breakWrongTwist vwt | vwt <- [ minBound .. maxBound ] ]
  where
    breakInpTwist :: Gate 'Standard -> [[Lit (NetworkSynthesis 'Standard)]]
    breakInpTwist g@(Gate i j k) = case k of
        0 -> []
        _ -> [ 
                [ NegLit (gate_ g)
                , toBetweenBeforeLit i j k
                , -unusedLit i (pred k) -- fixed line, this clause now subsumes breakParallelGates Earliest in a generalized encoding
                -- , unusedLit j (pred k) -- rather twist such that the used channel is i.. we don't really know whether both channels or only one of them is used in layer (pred k)
                ]
             ] -- ++
    breakWrongTwist :: ViaWrongTwist -> [[Lit (NetworkSynthesis 'Standard)]]
    breakWrongTwist vwt@(ViaWrongTwist p o k) = case k of
        0 -> [] -- ViaWrongTwist p o 0 is always negative
        _ -> [ NegLit $ viaWrongTwist_ vwt
             , -unusedLit o k
             , -unusedLit p k
             ] :
             [
                 [ NegLit $ viaWrongTwist_ vwt
                 , -gateLit (min o q) (max o q) k
                 , -unusedLit p k
                 ] 
             | q <- channels
             , q /= o
             , q /= p
             , min q o > p
             ] ++
             [
                 [ NegLit $ viaWrongTwist_ vwt
                 , -unusedLit o k
                 , -gateLit (min p r) (max p r) k
                 ] 
             | r <- channels
             , r /= o
             , r /= p
             , o > min r p
             ] ++
             [
                 [ NegLit $ viaWrongTwist_ vwt
                 , -gateLit (min o q) (max o q) k
                 , -gateLit (min p r) (max p r) k
                 ] 
             | q <- channels
             , q /= o
             , q /= p
             , r <- channels
             , r /= o
             , r /= p
             , min q o > min r p
             , q /= p
             , r /= o
             ] 
             
updateSR :: forall t. KnownNetType t => (Channel, Channel) -> [[Lit (NetworkSynthesis t)]]
updateSR (low, upp) = concat
    [ concat
        [ PosLit (gate_ g) `litImplies` fixGate g
        | g <- range (Gate low upp firstLayer, Gate low upp lastLayer)
        ]
    -- , 
    --     [ -- sorted relation is inferred backwards
    --         [  maxLit low j l
    --         ,  sortedRelLit (Value i k) ( inp j l)
    --         , -sortedRelLit (Value i k) (outp j l)
    --         ] -- not maxLit implies j is min channel or unused
    --     | k <- [ minBound .. maxBound ] :: [BetweenLayers]
    --     , l <- layers
    --     , Pair i j <- range (Pair low upp, Pair low upp) :: [Pair (AreGateChannelsOrdered t) 'NoDuplicates Channel]
    --     ]
    , 
        [ catMaybes
            [ maxLit' low j l
            , Just $ -sortedRelLit (inp  j l) (Value m k)
            , Just $  sortedRelLit (outp j l) (Value m k)
            ] -- not maxLit implies j is min channel or unused
        | l <- layers
        , k <- [ minBound .. maxBound ] :: [BetweenLayers]
        , Pair j m <- range (Pair low upp, Pair low upp) :: [Pair (AreGateChannelsOrdered t) 'NoDuplicates Channel]
        ]
    -- , 
    --     [ -- sorted relation is inferred backwards
    --         [  minLit i upp k
    --         ,  sortedRelLit (inp  i k) (Value j l)
    --         , -sortedRelLit (outp i k) (Value j l)
    --         ] -- not minLit implies i is max channel or unused
    --     | k <- layers
    --     , l <- [ minBound .. maxBound ] :: [BetweenLayers]
    --     , Pair i j <- range (Pair low upp, Pair low upp) :: [Pair (AreGateChannelsOrdered t) 'NoDuplicates Channel]
    --     ]
    , 
        [ catMaybes
            [ minLit' i upp k
            , Just $ -sortedRelLit (Value h l) (inp  i k)
            , Just $  sortedRelLit (Value h l) (outp i k)
            ] -- not minLit implies i is max channel or unused
        | k <- layers
        , l <- [ minBound .. maxBound ] :: [BetweenLayers]
        , Pair h i <- range (Pair low upp, Pair low upp) :: [Pair (AreGateChannelsOrdered t) 'NoDuplicates Channel]
        ]
    ]
  where
    fixGate :: KnownNetType t => Gate t -> [[Lit (NetworkSynthesis t)]]
    fixGate g = case g of
        Gate i j k -> 
            -- [ sortedRelLit (outp i k) (outp j k) ] : --delete this line, as it is inferred anyway?
            [ sortedRelLit (inp  i k) (outp j k) ] :
            [ sortedRelLit (outp i k)  (inp j k) ] : concat
            [ concat 
                [ 
                    [
                        [  sortedRelLit (outp i k) (Value m l)
                        , -sortedRelLit (inp  j k) (Value m l)
                        ]
                    ,
                        [  sortedRelLit (outp j k) (Value m l)
                        , -sortedRelLit (inp  i k) (Value m l)
                        , -sortedRelLit (inp  j k) (Value m l)
                        ]
                    ]
                | Pair j' m <- range (Pair low upp, Pair low upp) :: [Pair (AreGateChannelsOrdered t) 'NoDuplicates Channel]
                , j == j'
                , m /= i
                , l <- [ minBound .. maxBound ] :: [BetweenLayers]
                ]
            -- , -- sorted relation is inferred backwards
            --     [ 
            --         [ -sortedRelLit (Value h l) (outp i k)
            --         ,  sortedRelLit (Value h l) (inp  j k)
            --         ]
            --     | Pair h i' <- range (Pair low upp, Pair low upp) :: [Pair (AreGateChannelsOrdered t) 'NoDuplicates Channel]
            --     , i == i'
            --     , h /= j
            --     , l <- [ minBound .. maxBound ] :: [BetweenLayers]
            --     ]
            , concat
                [ 
                    [
                        [  sortedRelLit (Value h l) (outp j k)
                        , -sortedRelLit (Value h l) (inp  i k)
                        ]
                    ,
                        [  sortedRelLit (Value h l) (outp i k)
                        , -sortedRelLit (Value h l) (inp  i k)
                        , -sortedRelLit (Value h l) (inp  j k)
                        ]
                    ]
                | Pair h i' <- range (Pair low upp, Pair low upp) :: [Pair (AreGateChannelsOrdered t) 'NoDuplicates Channel]
                , i == i'
                , h /= j
                , l <- [ minBound .. maxBound ] :: [BetweenLayers]
                ]
            -- , -- sorted relation is inferred backwards
            --     [
            --         [ -sortedRelLit (outp j k) (Value m l)
            --         ,  sortedRelLit (inp  i k) (Value m l)
            --         ]
            --     | Pair j' m <- range (Pair low upp, Pair low upp) :: [Pair (AreGateChannelsOrdered t) 'NoDuplicates Channel]
            --     , j == j'
            --     , m /= i
            --     , l <- [ minBound .. maxBound ] :: [BetweenLayers]
            --     ]
            ]
    inp, outp :: Channel -> Layer -> Value
    inp  i k = Value i (before k)
    outp i k = Value i (after  k)

propagateSR :: forall t. KnownNetType t => [[Lit (NetworkSynthesis t)]]
propagateSR = concat
    [ 
        [ [ -sortedRelLit (Value i beforeFirstLayer) (Value j beforeFirstLayer)]
        | Pair i j <- [ minBound .. maxBound ] :: [Pair (AreGateChannelsOrdered t) 'NoDuplicates Channel]
        ]
    , updateSR (l, u)
    ]
  where
    l, u :: Channel
    (l, u) = (firstChannel, lastChannel)

banRedundantGates :: forall t. KnownNetType t => [[Lit (NetworkSynthesis t)]]
banRedundantGates =
    [ [ -sortedRelLit (Value i $ before k) (Value j $ before k), NegLit $ gate_ g ]
    | g@(Gate i j k) <- [ minBound .. maxBound ]
    ]

-- | WARNING: the legality of this constraint has not been proven
saturatedPrefixes :: forall t. KnownNetType t => [[Lit (NetworkSynthesis t)]]
saturatedPrefixes =
    [ [ sortedRelLit (Value i $ before k) (Value j $ before k), -unusedLit i k, -unusedLit j k ]
    | Gate i j k <- range
        ( Gate firstChannel lastChannel firstLayer
        , Gate firstChannel lastChannel lastLayer
        ) :: [Gate t]
    ]

updateH :: Word32 -> (Channel, Channel) -> [[Lit (NetworkSynthesis 'Standard)]]
updateH cexOffset (l, u) = concat
    [ concat
        [ fixGate g
        | g <- range (Gate l u firstLayer, Gate l u lastLayer)
        ]
    , 
        [ catMaybes
            [ maxLit' l j k
            , Just $  inp  j k
            , Just $ -outp j k
            ] -- not maxLit implies j is min channel or unused
        | j <- range (l, u)
        , k <- layers
        ]
    , 
        [ catMaybes 
            [ minLit' i u k
            , Just $ -inp  i k
            , Just $  outp i k
            ] -- not minLit implies i is max channel or unused
        | i <- range (l, u)
        , k <- layers
        ]
    ]
  where
    fixGate :: KnownNetType t => Gate t -> [[Lit (NetworkSynthesis t)]]
    fixGate g = case g of
        Gate i j k -> 
            [ [ NegLit $ gate_ g, outp i k, -inp j k, -inp i k ]
            , [ -sortedRelLit (Value i $ after k) (Value j $ before k), -outp i k, inp j k ]
            , [ NegLit $ gate_ g, -outp j k, inp j k, inp i k ]
            , [ -sortedRelLit (Value i $ before k) (Value j $ after k), outp j k,-inp i k ]
            ]
    inp, outp :: Channel -> Layer -> Lit (NetworkSynthesis t)
    inp  i k = PosLit (Value i (before k) cexOffset)
    outp i k = PosLit (Value i (after  k) cexOffset)
    
sortsH :: Word32 -> [Bool] -> [[Lit (NetworkSynthesis 'Standard)]]
sortsH cexOffset counterexample = concat
    [ zipWith fixValue cexFromL $ map value_ $ range (Value l beforeFirstLayer, Value u beforeFirstLayer)
    , updateH cexOffset (l, u)
    , zipWith fixValue (sort cexFromL) $ map value_ $ range (Value l afterLastLayer, Value u afterLastLayer)
    ]
  where
    l, u :: Channel
    -- (l, u) = (firstChannel, lastChannel)
    (l, u) = windowBounds counterexample
    cexFromL :: [Bool]
    cexFromL = genericDrop l counterexample
    fixValue :: Bool -> (Word32 -> NetworkSynthesis t) -> [Lit (NetworkSynthesis t)]
    fixValue polarity mkVal = [polarize polarity (mkVal cexOffset)]

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
    unusedLitsInFirstLayer = [ unusedLit i firstLayer | i <- channels ]


saturatedTwoLayerPrefix :: forall t. KnownNetType t => [[Lit (NetworkSynthesis t)]]
saturatedTwoLayerPrefix =
    [ [ gateLit i j firstLayer, -unusedLit i (succ firstLayer), -unusedLit j (succ firstLayer) ]
    | Pair i j <- range (Pair firstChannel lastChannel, Pair firstChannel lastChannel) :: [Pair (AreGateChannelsOrdered t) 'NoDuplicates Channel]
    ]


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
        | g <- range (Gate l u firstLayer, Gate l u lastLayer)
        ]
    , 
        [ catMaybes
            [ maxLit' l j k
            , Just $  inp  j k
            , Just $ -outp j k
            ] -- not maxLit implies j is min channel or unused
        | j <- range (l, u)
        , k <- layers
        ]
    , 
        [ catMaybes 
            [ minLit' i u k
            , Just $ -inp  i k
            , Just $  outp i k
            ] -- not minLit implies i is max channel or unused
        | i <- range (l, u)
        , k <- layers
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
    [ zipWith fixValue cexFromL $ map value_ $ range (Value l beforeFirstLayer, Value u beforeFirstLayer)
    , updateEM cexOffset (l, u)
    , zipWith fixValue (sort cexFromL) $ map value_ $ range (Value l afterLastLayer, Value u afterLastLayer)
    ]
  where
    l, u :: Channel
    -- (l, u) = (firstChannel, lastChannel)
    (l, u) = windowBounds counterexample
    cexFromL :: [Bool]
    cexFromL = genericDrop l counterexample
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


