{-# language PatternSynonyms #-}

module MinDepthSN.SAT.Synthesis.VarsBZ
    ( module Size
    , NetworkSynthesis(..)
    , pattern GateVar
    , pattern UnusedVar
    , pattern ValueVar
    , pattern GateOrUnusedVar
    , gateLit
    , unusedLit
    , gateOrUnusedLit
    , valueLit
    , trueAssignmentsOfGateOrUnusedVars
    , falseAssignmentsOfGateOrUnusedVars
    , trueAssignmentsOfGateVars
    , falseAssignmentsOfGateVars
    , trueAssignmentsOfUnusedVars
    , falseAssignmentsOfUnusedVars
    , trueAssignmentsOfValueVars
    , falseAssignmentsOfValueVars
    ) where

import SAT.IPASIR.EnumVars (Solver, Var(..), Lit(..), trueAssignmentsOfRange, falseAssignmentsOfRange)

import Numeric.Natural
import Enumerate.Enum.Valid (Validatable, isValid)
import MinDepthSN.Data.Size (Channel, Layer, BetweenLayers)
import MinDepthSN.Data.GateOrUnused (GateOrUnused(..), Gate, Unused)
import MinDepthSN.Data.Value (Value(..))

import qualified MinDepthSN.Data.Size as Size



-- network var
-- cexfix var
-- cex var



data NetworkSynthesis 
    = GateOrUnused_ { unGateOrUnused_ :: GateOrUnused }
    | Value_ { counterExIdx :: Natural, unValue_ :: Value }
    deriving (Eq, Ord)

-- | @GateVar i j k@ creates a variable \(g_{i,j}^k\) representing a
-- comparator gate where \(i\) and \(j\) are the channels and \(k\) is the layer.
pattern GateVar :: Channel -> Channel -> Layer -> Var NetworkSynthesis
pattern GateVar i j k = Var (GateOrUnused_ (Gate i j k))

-- | @UnusedVar i k@ creates a variable \(unused_i^k\) indicating a
-- channel \(i\) is not used by any comparator gate in layer \(k\).
pattern UnusedVar :: Channel -> Layer -> Var NetworkSynthesis
pattern UnusedVar i k = Var (GateOrUnused_ (Unused i k))

-- | @ValueVar x i k@ creates a variable \(v_i^k\) representing the value on
-- channel \(i\) between layers \(k-1\) and \(k\) for the x-th counterexample input.
pattern ValueVar :: Natural -> Channel -> BetweenLayers -> Var NetworkSynthesis
pattern ValueVar x i k = Var (Value_ x (Value i k))

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
pattern GateOrUnusedVar i j k = Var (GateOrUnused_ (GateOrUnused i j k))



instance Show NetworkSynthesis where
    show var = case var of
        GateOrUnused_ gu -> show gu
        Value_ cexIdx val -> show cexIdx ++ " " ++ show val

instance Validatable NetworkSynthesis where
    isValid var = case var of
        GateOrUnused_ gu -> isValid gu
        Value_ _ val -> isValid val

gateOrUnusedOffset :: Int
gateOrUnusedOffset = 0

valueOffset :: Int
valueOffset = fromEnum (GateOrUnused_ maxBound) + 1

-- | NetworkSynthesis values are enumerated as follows starting from 0:
-- where
--
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
        | i <= fromEnum (GateOrUnused_ maxBound) = GateOrUnused_ $ toEnum (i - gateOrUnusedOffset)
        | otherwise = Value_ (toEnum iter) (toEnum iVal)
        -- check if i < (2^n) * fromEnum (maxBound :: Value)
      where
        iter, iVal :: Int
        (iter, iVal) = (i - valueOffset) `quotRem` fromEnum (maxBound :: Value)

    fromEnum var = case var of
        GateOrUnused_ gu -> gateOrUnusedOffset + fromEnum gu
        Value_ iter val  -> valueOffset  + fromEnum val +
            fromEnum iter * (fromEnum (maxBound :: Value) + 1)


-- | Literal of 'GateOrUnusedVar' with positive polarity.
gateOrUnusedLit :: Channel -> Channel -> Layer -> Lit NetworkSynthesis
gateOrUnusedLit i j k = Pos (GateOrUnusedVar i j k)

pattern GateOrUnused_Var :: GateOrUnused -> Var NetworkSynthesis
pattern GateOrUnused_Var { gateOrUnused } = Var (GateOrUnused_ gateOrUnused)

minGateOrUnusedVar :: Var NetworkSynthesis
minGateOrUnusedVar = GateOrUnused_Var minBound

maxGateOrUnusedVar :: Var NetworkSynthesis
maxGateOrUnusedVar = GateOrUnused_Var maxBound

trueAssignmentsOfGateOrUnusedVars :: Solver s NetworkSynthesis [GateOrUnused]
trueAssignmentsOfGateOrUnusedVars =
    map gateOrUnused <$> trueAssignmentsOfRange minGateOrUnusedVar maxGateOrUnusedVar

falseAssignmentsOfGateOrUnusedVars :: Solver s NetworkSynthesis [GateOrUnused]
falseAssignmentsOfGateOrUnusedVars =
    map gateOrUnused <$> falseAssignmentsOfRange minGateOrUnusedVar maxGateOrUnusedVar


-- | Literal of 'GateVar' with positive polarity.
gateLit :: Channel -> Channel -> Layer -> Lit NetworkSynthesis
gateLit i j k = Pos (GateVar i j k)

pattern Gate_Var :: Gate -> Var NetworkSynthesis
pattern Gate_Var { gate } = Var (GateOrUnused_ (Gate_ gate))

minGateVar :: Var NetworkSynthesis
minGateVar = Gate_Var minBound

maxGateVar :: Var NetworkSynthesis
maxGateVar = Gate_Var maxBound

trueAssignmentsOfGateVars :: Solver s NetworkSynthesis [Gate]
trueAssignmentsOfGateVars = 
    map gate <$> trueAssignmentsOfRange minGateVar maxGateVar

falseAssignmentsOfGateVars :: Solver s NetworkSynthesis [Gate]
falseAssignmentsOfGateVars = 
    map gate <$> falseAssignmentsOfRange minGateVar maxGateVar


-- | Literal of 'UnusedVar' with positive polarity.
unusedLit :: Channel -> Layer -> Lit NetworkSynthesis
unusedLit i k = Pos (UnusedVar i k)

pattern Unused_Var :: Unused -> Var NetworkSynthesis
pattern Unused_Var { unused } = Var (GateOrUnused_ (Unused_ unused))

minUnusedVar :: Var NetworkSynthesis
minUnusedVar = Unused_Var minBound

maxUnusedVar :: Var NetworkSynthesis
maxUnusedVar = Unused_Var maxBound

trueAssignmentsOfUnusedVars :: Solver s NetworkSynthesis [Unused]
trueAssignmentsOfUnusedVars =
    map unused <$> trueAssignmentsOfRange minUnusedVar maxUnusedVar

falseAssignmentsOfUnusedVars :: Solver s NetworkSynthesis [Unused]
falseAssignmentsOfUnusedVars =
    map unused <$> falseAssignmentsOfRange minUnusedVar maxUnusedVar


-- | Literal of 'ValueVar' with positive polarity.
valueLit :: Natural -> Channel -> BetweenLayers -> Lit NetworkSynthesis
valueLit cexIdx i k = Pos (ValueVar cexIdx i k)

pattern Value_Var :: Natural -> Value -> Var NetworkSynthesis
pattern Value_Var { _cexIdx, value } = Var (Value_ _cexIdx value)

minValueVar :: Natural -> Var NetworkSynthesis
minValueVar cexIdx = Value_Var cexIdx minBound

maxValueVar :: Natural -> Var NetworkSynthesis
maxValueVar cexIdx = Value_Var cexIdx maxBound

trueAssignmentsOfValueVars :: Natural -> Solver s NetworkSynthesis [Value]
trueAssignmentsOfValueVars cexIdx =
    map value <$> trueAssignmentsOfRange (minValueVar cexIdx) (maxValueVar cexIdx)

falseAssignmentsOfValueVars :: Natural -> Solver s NetworkSynthesis [Value]
falseAssignmentsOfValueVars cexIdx =
    map value <$> falseAssignmentsOfRange (minValueVar cexIdx) (maxValueVar cexIdx)


