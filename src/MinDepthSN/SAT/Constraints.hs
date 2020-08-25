{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE FlexibleContexts #-}

module MinDepthSN.SAT.Constraints where

import Prelude hiding (negate, maximum, minimum)
import Data.List (nub)
import SAT.IPASIR (Var(..), Lit(..), negate)
import MinDepthSN.Data.Combinatorics2.CombinationNoRepetition (zipWithSuccs)
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
    minimum [in1, in2] outMin ++ maximum [in1, in2] outMax
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

-- | @minimum @\(l_{\min}\)@ [@\(l_0\)@,@\(l_1\)@,@\(\dots\)@,@\(l_k\)@]@ ensures that in a satisfying assignment the value of literal \(l_{\min}\) is the minimum of the values of all the literals \(l_0,l_1\) up to \(l_k\).
--
-- \[
-- \begin{aligned}
-- \mathtt{minimum}\ l_{\min}\ \{l_0, l_1, \dots, l_k\} ={} & \left(  l_{\min} \vee \neg l_0 \vee l_1 \vee \dots \vee \neg l_k \right)\\
--            & \wedge \left( \neg l_{\min} \vee l_0 \right)\\
--            & \wedge \left( \neg l_{\min} \vee l_1 \right)\\
--            & \dots\\
--            & \wedge \left( \neg l_{\min} \vee l_k \right)\\
-- \end{aligned}
-- \]
minimum :: [Lit a] -> Lit a -> [[Lit a]]
minimum lits minOfLits =
  (minOfLits : map negate lits) : map (\l -> [-minOfLits, l]) lits

-- | @maximum @\(l_{\max}\)@ [@\(l_0\)@,@\(l_1\)@,@\(\dots\)@,@\(l_k\)@]@ ensures that in a satisfying assignment the value of literal \(l_{\max}\) is the maximum of the values of all the literals \(l_0,l_1\) up to \(l_k\).
--
-- \[
-- \begin{aligned}
-- \mathtt{maximum}\ l_{\max}\ \{l_0, l_1, \dots, l_k\} ={} & \left( \neg l_{\max} \vee l_0 \vee l_1 \vee \dots \vee l_k \right)\\
--            & \wedge \left( l_{\max} \vee \neg l_0 \right)\\
--            & \wedge \left( l_{\max} \vee \neg l_1 \right)\\
--            & \dots\\
--            & \wedge \left( l_{\max} \vee \neg l_k \right)
-- \end{aligned}
-- \]
maximum :: [Lit a] -> Lit a -> [[Lit a]]
maximum lits maxOfLits =
  (-maxOfLits : lits) : map (\l -> [maxOfLits, -l]) lits

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
atMostOneOf = zipWithSuccs banCombin . nub
  where
    banCombin :: Lit a -> Lit a -> [Lit a]
    banCombin l1 l2 = [ -l1, -l2 ]

atLeastOneOf :: [Lit a] -> [[Lit a]]
atLeastOneOf lits = [ lits ]

noneOf :: [Lit a] -> [[Lit a]]
noneOf lits = [ [ -l ] | l <- lits ] 

allOf :: [Lit a] -> [[Lit a]]
allOf lits = [ [ l ] | l <- lits ]