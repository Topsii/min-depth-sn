{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}

module MinDepthSN.SAT.Synthesis.Variables
    ( module Size
    -- * Variables and literals for network synthesization
    , NetworkSynthesis(..)
    , pattern GateVar
    , pattern UnusedVar
    , pattern ValueVar
    , pattern GateOrUnusedVar
    , GateOrUnused
    , pattern GateOrUnused
    , Data.Value(Value)
    , Data.Gate(Gate)
    , Data.Unused(Unused)
    , compLit--, isGate
    , unusedLit--, isUnused
    , compOrUnusedLit
    , minGateVar, maxGateVar
    , minUnusedVar, maxUnusedVar
    , minValueVar, maxValueVar
    , trueAssignmentsOfGateOrUnusedVars
    , falseAssignmentsOfGateOrUnusedVars
    , trueAssignmentsOfGateVars
    , falseAssignmentsOfGateVars
    , trueAssignmentsOfUnusedVars
    , falseAssignmentsOfUnusedVars
    , trueAssignmentsOfValueVars
    , falseAssignmentsOfValueVars
    --, minChannel, maxChannel
    --, minLayer, maxLayer
    -- * Variables and literals for finding a counter example
    --, Value
    --, valueVar, valueLit
    --, minValue, maxValue
    ) where

import SAT.IPASIR.EnumVars (Solver, Var(..), Lit(..), trueAssignmentsOfRange, falseAssignmentsOfRange)

import Numeric.Natural
import Enumerate.Enum.Valid (Validatable, isValid)
import MinDepthSN.Data.Size
import MinDepthSN.Data.Gate (Gate)
import qualified MinDepthSN.Data.Gate as Data
import MinDepthSN.Data.Unused (Unused)
import qualified MinDepthSN.Data.Unused as Data
import MinDepthSN.Data.Value (Value)
import qualified MinDepthSN.Data.Value as Data

import qualified MinDepthSN.Data.Size as Size

--data Val = Val Channel Layer
--data Unused = Unused Channel Layer

--data InitVar = InitVarGate Gate | InitVarUnused Unused

--data IterVar = IterVarVal Val

--data Var = InitVar InitVar | IterVar Word IterVar


-- network var
-- cexfix var
-- cex var






{-# COMPLETE GateVar, UnusedVar, ValueVar #-}
{-# COMPLETE GateOrUnusedVar, ValueVar #-}

data NetworkSynthesis 
    = Gate_ { unGate_ :: Gate }
    | Unused_ { unUnused_ :: Unused }
    | Value_ { counterExIdx :: Natural, unValue_ :: Value }
    deriving (Eq, Ord)

-- | @GateVar i j k@ creates a variable \(g_{i,j}^k\) representing a
-- comparator gate where \(i\) and \(j\) are the channels and \(k\) is the layer.
pattern GateVar :: Channel -> Channel -> Layer -> Var NetworkSynthesis
pattern GateVar i j k = Var (Gate_ (Data.Gate i j k))

-- | @UnusedVar i k@ creates a variable \(unused_i^k\) indicating a
-- channel \(i\) is not used by any comparator gate in layer \(k\).
pattern UnusedVar :: Channel -> Layer -> Var NetworkSynthesis
pattern UnusedVar i k = Var (Unused_ (Data.Unused i k))

-- | @ValueVar x i k@ creates a variable \(v_i^k\) representing the value on
-- channel \(i\) between layers \(k-1\) and \(k\) for the x-th counterexample input.
pattern ValueVar :: Natural -> Channel -> BetweenLayers -> Var NetworkSynthesis
pattern ValueVar x i k = Var (Value_ x (Data.Value i k))

-- | A variable \(gu_{i,j}^k\) is either a comparator \(g\)ate variable or an 
-- \(u\)nused variable.
--
-- \[
-- gu_{i,j}^k :=    
--      \begin{cases}
--          g_{j,i}^k  & \text{if}\ i > j \\
--          g_{i,j}^k  & \text{if}\ i < j \\
--          unused_i^k & \text{otherwise}
--      \end{cases}
-- \]
--
-- For \(g_{i,j}^k\) see 'compVar' and for \(unused_i^k\) see 'unusedVar'
--
-- This enables a terser definition of the \(used\), \(once\) and \(update\) constraints
-- from the original encoding of Bundala and Zavodny.
-- See 'MinDepthSN.SAT.Synthesis.Constraints.usageOnce' and 'MinDepthSN.SAT.Synthesis.Constraints.unused'
--
-- The solver does not actually create, consider or solve \(gu_{i,j}^k\) 
-- variables. For any occurrence of \(gu_{i,j}^k\) in formulas or clauses, its 
-- definition is passed to the solver instead.
pattern GateOrUnusedVar :: Channel -> Channel -> Layer -> Var NetworkSynthesis
-- wait for or-patterns: https://github.com/ghc-proposals/ghc-proposals/pull/43
-- get rid of matchGateOrUnused and view patterns then
--pattern GateOrUnusedVar i j k <- GateVar i j k | GateOrUnusedVar i i k <- UnusedVar i k
pattern GateOrUnusedVar i j k <- (matchGateOrUnused -> (i, j, k)) where
    GateOrUnusedVar i j k
        | i > j = GateVar j i k
        | i < j = GateVar i j k
        | otherwise = UnusedVar i k

{-# COMPLETE GateOrUnused #-}

newtype GateOrUnused = GateOrUnused_ { unGateOrUnused_ :: NetworkSynthesis }
    deriving (Eq, Ord)

instance Show GateOrUnused where
    show = show . unGateOrUnused_

pattern GateOrUnused :: Channel -> Channel -> Layer -> GateOrUnused
pattern GateOrUnused i j k <- (matchGateOrUnused . Var . unGateOrUnused_ -> (i, j, k)) where
    GateOrUnused i j k = GateOrUnused_ . unVar $ GateOrUnusedVar i j k

-- obsolete with or-patterns, see above
matchGateOrUnused ::  Var NetworkSynthesis -> (Channel, Channel, Layer)
matchGateOrUnused v
    | isGate v || isUnused v = (i, j, k)
  --  | otherwise = error $ "matchGateOrUnused: Not a comp or unused variable: " ++ show v
  where
    i, j :: Channel
    i = minChannel v
    j = maxChannel v
    k :: Layer
    k = minLayer v

isGate :: Var NetworkSynthesis -> Bool
isGate v = case v of
    GateVar {} -> True
    _          -> False

isUnused :: Var NetworkSynthesis -> Bool
isUnused v = case v of
    UnusedVar {} -> True
    _            -> False

minChannel :: Var NetworkSynthesis -> Channel
minChannel v = case v of
    GateVar i j _ -> min i j
    UnusedVar i _ -> i
    ValueVar _ i _  -> i

maxChannel :: Var NetworkSynthesis -> Channel
maxChannel v = case v of
    GateVar i j _ -> max i j
    UnusedVar i _ -> i
    ValueVar _ i _  -> i
    
minLayer :: Var NetworkSynthesis -> Layer
minLayer v = case v of
    GateVar _ _ k -> k
    UnusedVar _ k -> k
    ValueVar {}  -> error $ "minLayer: ValueVar " ++ show v ++ 
        "is between layers and thus has no layer." 

-- maxLayer :: Var NetworkSynthesis -> Layer
-- maxLayer v = case v of
--     GateVar _ _ k -> k
--     UnusedVar _ k -> k
--     ValueVar {}  -> error $ "minLayer: ValueVar " ++ show v ++ 
--         "is between layers and thus has no layer." 

instance Show NetworkSynthesis where
    show var = case var of
        Gate_ c -> show c
        Unused_ u -> show u
        Value_ cexIdx val -> show cexIdx ++ " " ++ show val

instance Validatable NetworkSynthesis where
    isValid var = case var of
        Gate_ c -> isValid c
        Unused_ u -> isValid u
        Value_ _ val -> isValid val

compOffset :: Int
compOffset = 0

unusedOffset :: Int
unusedOffset = fromEnum (Gate_ maxBound) + 1

valueOffset :: Int
valueOffset = fromEnum (Unused_ maxBound) + 1

-- | NetworkSynthesis values are enumerated as follows starting from 0:
-- where
--    0   <-> Gate_ minBound
--    ... <-> ...
--    ... <-> Gate_ maxBound
--    ... <-> Unused_ minBound
--    ... <-> ...
--    ... <-> Unused_ maxBound
--    ... <-> Value_ 0 minBound
--    ... <-> ...
--    ... <-> Value_ 0 maxBound
--    ... <-> Value_ 1 minBound
--    ... <-> ...
--    ... <-> Value_ 1 maxBound
--    ... <-> ...
instance Enum NetworkSynthesis where

    toEnum i
        | i < 0 = error $ "toEnum (NetworkSynthesis): negative argument " ++ show i
        | i <= fromEnum (Gate_ maxBound) = Gate_ $ toEnum (i - compOffset)
        | i <= fromEnum (Unused_ maxBound) = Unused_ $ toEnum (i - unusedOffset)
        | otherwise = Value_ (toEnum iter) (toEnum iVal)
        -- check if i < (2^n) * fromEnum (maxBound :: Value)
      where
        iter, iVal :: Int
        (iter, iVal) = (i - valueOffset) `quotRem` fromEnum (maxBound :: Value)

    fromEnum var = case var of
        Gate_ c         -> compOffset   + fromEnum c
        Unused_ u       -> unusedOffset + fromEnum u
        Value_ iter val -> valueOffset  + fromEnum val +
            fromEnum iter * (fromEnum (maxBound :: Value) + 1)
            



-- instance Ord NetworkVar where
--     compare = 
--         comparing isGate <> 
--         comparing isUnused












-- | Literal of 'GateOrUnusedVar' with positive polarity.
compOrUnusedLit :: Channel -> Channel -> Layer -> Lit NetworkSynthesis
compOrUnusedLit i j k = Pos (GateOrUnusedVar i j k)

minGateOrUnusedVar :: Var NetworkSynthesis
minGateOrUnusedVar = Var (min (Gate_ minBound) (Unused_ minBound))

maxGateOrUnusedVar :: Var NetworkSynthesis
maxGateOrUnusedVar = Var (max (Gate_ maxBound) (Unused_ maxBound))

trueAssignmentsOfGateOrUnusedVars :: Solver s NetworkSynthesis [GateOrUnused]
trueAssignmentsOfGateOrUnusedVars = map (GateOrUnused_ . unVar) <$> trueAssignmentsOfRange minGateOrUnusedVar maxGateOrUnusedVar

falseAssignmentsOfGateOrUnusedVars :: Solver s NetworkSynthesis [GateOrUnused]
falseAssignmentsOfGateOrUnusedVars = map (GateOrUnused_ . unVar) <$> falseAssignmentsOfRange minGateOrUnusedVar maxGateOrUnusedVar


-- | Literal of 'GateVar' with positive polarity.
compLit :: Channel -> Channel -> Layer -> Lit NetworkSynthesis
compLit i j k = Pos (GateVar i j k)

minGateVar :: Var NetworkSynthesis
minGateVar = Var $ Gate_ minBound

maxGateVar :: Var NetworkSynthesis
maxGateVar = Var $ Gate_ maxBound

trueAssignmentsOfGateVars :: Solver s NetworkSynthesis [Gate]
trueAssignmentsOfGateVars = map (unGate_ . unVar) <$> trueAssignmentsOfRange minGateVar maxGateVar

falseAssignmentsOfGateVars :: Solver s NetworkSynthesis [Gate]
falseAssignmentsOfGateVars = map (unGate_ . unVar) <$> falseAssignmentsOfRange minGateVar maxGateVar


unusedLit :: Channel -> Layer -> Lit NetworkSynthesis
unusedLit i k = Pos (UnusedVar i k)

minUnusedVar :: Var NetworkSynthesis
minUnusedVar = Var $ Unused_ minBound

maxUnusedVar :: Var NetworkSynthesis
maxUnusedVar = Var $ Unused_ maxBound

trueAssignmentsOfUnusedVars :: Solver s NetworkSynthesis [Unused]
trueAssignmentsOfUnusedVars = map (unUnused_ . unVar) <$> trueAssignmentsOfRange minUnusedVar maxUnusedVar

falseAssignmentsOfUnusedVars :: Solver s NetworkSynthesis [Unused]
falseAssignmentsOfUnusedVars = map (unUnused_ . unVar) <$> falseAssignmentsOfRange minUnusedVar maxUnusedVar



minValueVar :: Natural -> Var NetworkSynthesis
minValueVar cexIdx = Var $ Value_ cexIdx minBound

maxValueVar :: Natural -> Var NetworkSynthesis
maxValueVar cexIdx = Var $ Value_ cexIdx maxBound

trueAssignmentsOfValueVars :: Natural -> Solver s NetworkSynthesis [Value]
trueAssignmentsOfValueVars cexIdx = map (unValue_ . unVar) <$> trueAssignmentsOfRange (minValueVar cexIdx) (maxValueVar cexIdx)

falseAssignmentsOfValueVars :: Natural -> Solver s NetworkSynthesis [Value]
falseAssignmentsOfValueVars cexIdx = map (unValue_ . unVar) <$> falseAssignmentsOfRange (minValueVar cexIdx) (maxValueVar cexIdx)


{-
compVar :: Channel -> Channel -> Layer -> Var Gate
compVar i j k
    | i < j = Var $ Gate i j k
    | i > j = Var $ Gate j i k

data Gate = Gate Channel -- lower indexed channel
                 Channel -- higher indexed channel
                 Layer
    deriving (Show, Eq, Ord, Generic, Enumerable)

instance Validatable Gate where
    isValid (Gate i j _)
        | i < j = True
        | otherwise = False



data Used = UsedUnexported Channel Layer
    deriving (Show, Eq, Ord, Generic, Enumerable)

instance Bounded Used where
    minBound = minUsed
    maxBound = maxUsed

instance Validatable Used where
    isValid = const True

instance Enum Used where
    toEnum   = toEnum_enumerable   arrayUsed
    fromEnum = fromEnum_enumerable tableUsed
    
tableUsed :: Map Used Int
tableUsed = tableEnumerable

arrayUsed :: Array Int Used
arrayUsed = arrayEnumerable

maxUsed :: Used
maxUsed = validMaxBound

minUsed :: Used
minUsed = validMinBound
-}















{-

data Value = Value Channel BetweenLayers
    deriving (Show, Eq, Ord, Generic, Enumerable)

instance Bounded Value where
    minBound = minValue
    maxBound = maxValue

instance Validatable Value where
    isValid = const True

instance Enum Value where
    toEnum   = toEnum_enumerable   arrayValue
    fromEnum = fromEnum_enumerable tableValue

tableValue :: Map Value Int
tableValue = tableEnumerable

arrayValue :: Array Int Value
arrayValue = arrayEnumerable

valueLit :: Channel -> BetweenLayers -> Lit Value
valueLit i k = Pos (valueVar i k)

valueVar :: Channel -> BetweenLayers -> Var Value
valueVar i k = Var $ Value i k

maxValue :: Value
maxValue = validMaxBound

minValue :: Value
minValue = validMinBound
-}
