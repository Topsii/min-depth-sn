{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE FlexibleContexts #-}

module MinDepthSN.SAT.Constraints where

import Prelude hiding (negate, maximum, minimum)
import Data.List (inits, nub)
import SAT.IPASIR (Var(..), Lit(..), negate)
import MinDepthSN.Data.Size (Channel, BetweenLayers, before, after)
import MinDepthSN.Data.Value (Value(..))
import MinDepthSN.Data.GateOrUnused (GateOrUnused(..))


-- | @fixGateOrUnused (GateOrUnused i j k)@ either compares the values on the 
-- channels \(i\) and \(j\) in layer \(k\) if \(i<j\) or does not use it if 
-- \(i=j\).
--
-- For the definition of \(gu\) see 'MinDepthSN.Data.GateOrUnused.GateOrUnused'.
--
-- \[
-- \left( v_i^{k+1} \leftrightarrow \left( v_i^k \wedge v_j^k \right) \right)
-- \wedge \\
-- \left( v_j^{k+1} \leftrightarrow \left( v_i^k \vee v_j^k \right) \right) \\
-- \]
--
-- See 'minimum' and 'maximum' for the CNF.
--
fixGateOrUnused :: GateOrUnused -> [[Lit Value]]
fixGateOrUnused (GateOrUnused i j k) =
    minimum in1 in2 outMin ++ maximum in1 in2 outMax
  where
    beforeK, afterK :: BetweenLayers
    beforeK = before k
    afterK = after k
    in1, in2, outMin, outMax :: Lit Value
    in1 = valueLit i beforeK
    in2 = valueLit j beforeK
    outMin = valueLit i afterK
    outMax = valueLit j afterK

valueLit :: Channel -> BetweenLayers -> Lit Value
valueLit i k = Positive $ Var $ Value i k

-- | @minimum a b min@ ensures \(a \wedge b \leftrightarrow min\).
--
-- @a@ and @b@ may be comparator inputs and @min@ the output on the lower
-- indexed channel.
--
-- \[
-- \left( min \vee \neg a \vee \neg b \right)
-- \wedge \left( \neg min \vee a \right)
-- \wedge \left( \neg min \vee b \right)
-- \]
minimum :: Lit a -> Lit a -> Lit a -> [[Lit a]]
minimum lit1 lit2 litMin =
    [ [ litMin, -lit1, -lit2]
    , [-litMin,  lit1]
    , [-litMin,  lit2]
    ]

-- | @maximum a b max@ ensures \(a \vee b \leftrightarrow max\).
--
-- @a@ and @b@ may be comparator inputs and @max@ the output on the higher
-- indexed channel.
--
-- \[
-- \left( \neg max \vee a \vee b \right)
-- \wedge \left( max \vee \neg a \right)
-- \wedge \left( max \vee \neg b \right)
-- \]
maximum :: Lit a -> Lit a -> Lit a -> [[Lit a]]
maximum lit1 lit2 litMax =
    [ [-litMax,  lit1,  lit2]
    , [ litMax, -lit1]
    , [ litMax, -lit2]
    ]

-- | @litImplies premissLit clauses@ ensures \(premissLit \rightarrow clauses\).
--
-- In detail it transforms clauses of the form 
-- \[\bigwedge_{clause \in clauses} clause\] 
-- to
-- \[
-- \bigwedge_{clause \in clauses} 
--     \left( \neg premissLit \vee clause \right)
-- \]
litImplies :: Lit a -> [[Lit a]] -> [[Lit a]]
litImplies = map . (:) . negate

exactlyOneOf :: Eq a => [Lit a] -> [[Lit a]]
exactlyOneOf lits = atLeastOneOf lits ++ atMostOneOf lits

-- | Let \(lit_1\) and \(lit_2\) be two literals from the list. They cannot both 
-- be true, since it would contradict the clause 
-- \( \neg lit_1 \vee \neg lit_2 \).
atMostOneOf :: forall a. Eq a => [Lit a] -> [[Lit a]]
atMostOneOf lits =
    [
        [ -l1, -l2 ]
    | (litsBeforeL2, l2) <- zip (inits uniqueLits) uniqueLits
    , l1 <- litsBeforeL2
    ]
  where
    uniqueLits :: [Lit a]
    uniqueLits = nub lits

atLeastOneOf :: [Lit a] -> [[Lit a]]
atLeastOneOf lits = [ lits ]

noneOf :: [Lit a] -> [[Lit a]]
noneOf lits = [ [ -l ] | l <- lits ] 

allOf :: [Lit a] -> [[Lit a]]
allOf lits = [ [ l ] | l <- lits ]