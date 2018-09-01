{-# language PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE RankNTypes #-}

module MinDepthSN.SAT.Synthesis.VarsBZ
    ( module Size
    , NetworkSynthesis(..)
    , StandardNetworkSynthesis
    , GeneralizedNetworkSynthesis
    , pattern GateVar
    , pattern UnusedVar
    , pattern ValueVar
    , pattern GateOrUnusedVar
    , gateLit
    , unusedLit
    , gateOrUnusedLit
    , valueLit
    , trueAssignedGateOrUnusedVars
    , falseAssignedGateOrUnusedVars
    , trueAssignedGateVars
    , falseAssignedGateVars
    , trueAssignedUnusedVars
    , falseAssignedUnusedVars
    , assignmentsOfValueVars
    , trueAssignedValueVars
    , falseAssignedValueVars
    ) where

import SAT.IPASIR.EnumVars (Solver, Var(..), Lit(..), trueAssignedVarsOfRange, falseAssignedVarsOfRange, assignmentsOfRange)

import Numeric.Natural
import MinDepthSN.Data.Size (Channel, Layer, BetweenLayers)
import MinDepthSN.Data.GateOrUnused 
    ( GateOrUnused(..)
    , Gate
    , Unused
    , SortingOrder(..)
    , SortOrder
    )
import MinDepthSN.Data.Value (Value(..))

import qualified MinDepthSN.Data.Size as Size

-- import Data.Profunctor.Choice

-- import Data.Profunctor
-- import Control.Lens.Prism


-- type Prism s t a b = forall p f. (Choice p) => p a (f b) -> p s (f t)
-- type Prism' s a = Prism s s a a

-- _unused :: (Choice p) => p (GateOrUnused o) (f (GateOrUnused o)) -> p (NetworkSynthesis o) (f (NetworkSynthesis o))
-- _unused :: Prism' (NetworkSynthesis o) (GateOrUnused o)
-- _unused = dimap to from . right'
--   where
--     to (GateOrUnused_ u) = Right u
--     to x                 = Left x
--     from (Left it)       = pure it
--     from (Right fa)      = GateOrUnused_ <$> fa

-- network var
-- cexfix var
-- cex var


type StandardNetworkSynthesis = NetworkSynthesis 'Standard
type GeneralizedNetworkSynthesis = NetworkSynthesis 'Generalized

data NetworkSynthesis (o :: SortingOrder) 
    = GateOrUnused_ { unGateOrUnused_ :: GateOrUnused o }
    | Value_ { counterExIdx :: Natural, unValue_ :: Value }
    deriving (Eq, Ord)

-- | @GateVar i j k@ creates a variable \(g_{i,j}^k\) representing a
-- comparator gate where \(i\) and \(j\) are the channels and \(k\) is the layer.
pattern GateVar :: SortOrder o => Channel -> Channel -> Layer -> Var (NetworkSynthesis o)
pattern GateVar i j k = Var (GateOrUnused_ (Gate i j k))

-- | @UnusedVar i k@ creates a variable \(unused_i^k\) indicating a
-- channel \(i\) is not used by any comparator gate in layer \(k\).
pattern UnusedVar :: Channel -> Layer -> Var (NetworkSynthesis o)
pattern UnusedVar i k = Var (GateOrUnused_ (Unused i k))

-- | @ValueVar x i k@ creates a variable \(v_i^k\) representing the value on
-- channel \(i\) between layers \(k-1\) and \(k\) for the x-th counterexample input.
pattern ValueVar :: Natural -> Channel -> BetweenLayers -> Var (NetworkSynthesis o)
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
pattern GateOrUnusedVar :: SortOrder o => Channel -> Channel -> Layer -> Var (NetworkSynthesis o)
pattern GateOrUnusedVar i j k = Var (GateOrUnused_ (GateOrUnused i j k))



instance SortOrder o => Show (NetworkSynthesis o) where
    show var = case var of
        GateOrUnused_ gu -> show gu
        Value_ cexIdx val -> show cexIdx ++ " " ++ show val

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
instance SortOrder o => Enum (NetworkSynthesis o) where

    toEnum i
        | i < 0 = error $ "toEnum (NetworkSynthesis): negative argument " ++ show i
        | i <= fromEnum (GateOrUnused_ maxBound :: NetworkSynthesis o) = GateOrUnused_ $ toEnum (i - gateOrUnusedOffset)
        | otherwise = Value_ (toEnum iter) (toEnum iVal)
        -- check if i < (2^n) * fromEnum (maxBound :: Value)
      where
        iter, iVal :: Int
        (iter, iVal) = (i - valueOffset) `quotRem` fromEnum (maxBound :: Value)
        gateOrUnusedOffset :: Int
        gateOrUnusedOffset = 0
        valueOffset :: Int
        valueOffset = fromEnum (GateOrUnused_ maxBound :: NetworkSynthesis o) + 1

    fromEnum var = case var of
        GateOrUnused_ gu -> gateOrUnusedOffset + fromEnum gu
        Value_ iter val  -> valueOffset  + fromEnum val +
            fromEnum iter * (fromEnum (maxBound :: Value) + 1)
      where
        gateOrUnusedOffset :: Int
        gateOrUnusedOffset = 0
        valueOffset :: Int
        valueOffset = fromEnum (GateOrUnused_ maxBound :: NetworkSynthesis o) + 1


-- | Literal of 'GateOrUnusedVar' with positive polarity.
gateOrUnusedLit :: SortOrder o => Channel -> Channel -> Layer -> Lit (NetworkSynthesis o)
gateOrUnusedLit i j k = Pos (GateOrUnusedVar i j k)

pattern GateOrUnused_Var :: GateOrUnused o -> Var (NetworkSynthesis o)
pattern GateOrUnused_Var { gateOrUnused } = Var (GateOrUnused_ gateOrUnused)

minGateOrUnusedVar :: SortOrder o => Var (NetworkSynthesis o)
minGateOrUnusedVar = GateOrUnused_Var minBound

maxGateOrUnusedVar :: SortOrder o => Var (NetworkSynthesis o)
maxGateOrUnusedVar = GateOrUnused_Var maxBound

trueAssignedGateOrUnusedVars :: SortOrder o => Solver s (NetworkSynthesis o) [GateOrUnused o]
trueAssignedGateOrUnusedVars =
    map gateOrUnused <$> trueAssignedVarsOfRange minGateOrUnusedVar maxGateOrUnusedVar

falseAssignedGateOrUnusedVars :: SortOrder o => Solver s (NetworkSynthesis o) [GateOrUnused o]
falseAssignedGateOrUnusedVars =
    map gateOrUnused <$> falseAssignedVarsOfRange minGateOrUnusedVar maxGateOrUnusedVar


-- | Literal of 'GateVar' with positive polarity.
gateLit :: SortOrder o => Channel -> Channel -> Layer -> Lit (NetworkSynthesis o)
gateLit i j k = Pos (GateVar i j k)

pattern Gate_Var :: Gate o -> Var (NetworkSynthesis o)
pattern Gate_Var { gate } = Var (GateOrUnused_ (Gate_ gate))

minGateVar :: SortOrder o => Var (NetworkSynthesis o)
minGateVar = Gate_Var minBound

maxGateVar :: SortOrder o => Var (NetworkSynthesis o)
maxGateVar = Gate_Var maxBound

trueAssignedGateVars :: SortOrder o => Solver s (NetworkSynthesis o) [Gate o]
trueAssignedGateVars = 
    map gate <$> trueAssignedVarsOfRange minGateVar maxGateVar

falseAssignedGateVars :: SortOrder o => Solver s (NetworkSynthesis o) [Gate o]
falseAssignedGateVars = 
    map gate <$> falseAssignedVarsOfRange minGateVar maxGateVar


-- | Literal of 'UnusedVar' with positive polarity.
unusedLit :: Channel -> Layer -> Lit (NetworkSynthesis o)
unusedLit i k = Pos (UnusedVar i k)

pattern Unused_Var :: Unused -> Var (NetworkSynthesis o)
pattern Unused_Var { unused } = Var (GateOrUnused_ (Unused_ unused))

minUnusedVar :: Var (NetworkSynthesis o)
minUnusedVar = Unused_Var minBound

maxUnusedVar :: Var (NetworkSynthesis o)
maxUnusedVar = Unused_Var maxBound

trueAssignedUnusedVars :: SortOrder o => Solver s (NetworkSynthesis o) [Unused]
trueAssignedUnusedVars =
    map unused <$> trueAssignedVarsOfRange minUnusedVar maxUnusedVar

falseAssignedUnusedVars :: SortOrder o => Solver s (NetworkSynthesis o) [Unused]
falseAssignedUnusedVars =
    map unused <$> falseAssignedVarsOfRange minUnusedVar maxUnusedVar


-- | Literal of 'ValueVar' with positive polarity.
valueLit :: Natural -> Channel -> BetweenLayers -> Lit (NetworkSynthesis o)
valueLit cexIdx i k = Pos (ValueVar cexIdx i k)

pattern Value_Var :: Natural -> Value -> Var (NetworkSynthesis o)
pattern Value_Var { _cexIdx, value } = Var (Value_ _cexIdx value)

minValueVar :: Natural -> Var (NetworkSynthesis o)
minValueVar cexIdx = Value_Var cexIdx minBound

maxValueVar :: Natural -> Var (NetworkSynthesis o)
maxValueVar cexIdx = Value_Var cexIdx maxBound

assignmentsOfValueVars :: SortOrder o => Natural -> Solver s (NetworkSynthesis o) [Bool]
assignmentsOfValueVars cexIdx = assignmentsOfRange (minValueVar cexIdx) (maxValueVar cexIdx)

trueAssignedValueVars :: SortOrder o => Natural -> Solver s (NetworkSynthesis o) [Value]
trueAssignedValueVars cexIdx =
    map value <$> trueAssignedVarsOfRange (minValueVar cexIdx) (maxValueVar cexIdx)

falseAssignedValueVars :: SortOrder o => Natural -> Solver s (NetworkSynthesis o) [Value]
falseAssignedValueVars cexIdx =
    map value <$> falseAssignedVarsOfRange (minValueVar cexIdx) (maxValueVar cexIdx)


