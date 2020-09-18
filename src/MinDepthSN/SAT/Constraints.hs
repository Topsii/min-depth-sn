{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE FlexibleContexts #-}

module MinDepthSN.SAT.Constraints where

import Prelude hiding (negate, maximum, minimum)
import Data.List (nub)
import Data.Pair.UnorderedNoDuplicates (zipWithSuccs)
import SAT.IPASIR (Lit(..), negate)
import MinDepthSN.Data.Size (Layer, Channel, BetweenLayers, before, after)
import MinDepthSN.Data.Value (Value(..))
import MinDepthSN.Data.GateOrUnused (GateOrUnused(..))
import MinDepthSN.Data.Gate (Gate(..), KnownNetType)
import MinDepthSN.Data.Unused (Unused(..))


-- | @fixGateOrUnused (GateOrUnused i j k)@ either compares the values on the 
-- channels \(i\) and \(j\) in layer \(k\) if \(i<j\) or does not use it if 
-- \(i=j\).
--
-- For the definition of \(gu\) see 'MinDepthSN.Data.GateOrUnused.GateOrUnused'.
--
-- For gates between channels \(i\) and \(j\) in layer \(k\) we have:
-- \[
-- \left( v_i^{k+1} \leftrightarrow \left( v_i^k \wedge v_j^k \right) \right)
-- \wedge \\
-- \left( v_j^{k+1} \leftrightarrow \left( v_i^k \vee v_j^k \right) \right) \\
-- \]
--
-- For a channel \(i\) in layer \(k\) that is not used by any comparator we
-- have:
-- \[
-- v_i^{k+1} \leftrightarrow v_i^k
-- \]
-- See 'iff', 'iffDisjunctionOf' and 'iffConjunctionOf' for the CNF.
--
fixGateOrUnused :: KnownNetType t => GateOrUnused t -> [[Lit Value]]
fixGateOrUnused gu = case gu of
    Gate_   (Gate i j k) -> outp i k `iffConjunctionOf` [inp i k, inp j k]
                         ++ outp j k `iffDisjunctionOf` [inp i k, inp j k]
    Unused_ (Unused i k) -> outp i k `iff`               inp i k
  where
    inp, outp :: Channel -> Layer -> Lit Value
    inp  i k = valueLit i (before k)
    outp i k = valueLit i (after  k)

valueLit :: Channel -> BetweenLayers -> Lit Value
valueLit i k = PosLit $ Value i k

-- | \(l_{c}\)@ \`iffConjunctionOf\` [@\(l_0\)@,@\(l_1\)@,@\(\dots\)@,@\(l_k\)@]@
-- ensures that in a satisfying assignment the value of literal \(l_{c}\) is
-- the conjunction the values of all the literals \(l_0,l_1,\dots\) up to \(l_k\).
--
-- \[
-- \begin{aligned}
-- \mathtt{iffConjunctionOf}\ l_{c}\ \{l_0, l_1, \dots, l_k\} ={}
--  & l_c \leftrightarrow \bigwedge_{i \in 0,\dots,k} l_i\\ ={}
--  & \left(  l_{c} \vee \neg l_0 \vee \neg l_1 \vee \dots \vee \neg l_k \right)\\
--  & \wedge \left( \neg l_{c} \vee l_0 \right)\\
--  & \wedge \left( \neg l_{c} \vee l_1 \right)\\
--  & \dots\\
--  & \wedge \left( \neg l_{c} \vee l_k \right)\\
-- \end{aligned}
-- \]
iffConjunctionOf :: Lit a -> [Lit a] -> [[Lit a]]
iffConjunctionOf conjOfLits lits =
  (conjOfLits : map negate lits) : map (\l -> [-conjOfLits, l]) lits

-- | \(l_{d}\)@ \`iffDisjunctionOf\` [@\(l_0\)@,@\(l_1\)@,@\(\dots\)@,@\(l_k\)@]@
-- ensures that in a satisfying assignment the value of literal \(l_{d}\) is
-- the disjunction of the values of all the literals \(l_0,l_1,\dots\) up to \(l_k\).
--
-- \[
-- \begin{aligned}
-- \mathtt{iffDisjunctionOf}\ l_{d}\ \{l_0, l_1, \dots, l_k\} ={}
--  & l_d \leftrightarrow \bigvee_{i \in 0,\dots,k} l_i\\ ={}
--  & \left( \neg l_{d} \vee l_0 \vee l_1 \vee \dots \vee l_k \right)\\
--  & \wedge \left( l_{d} \vee \neg l_0 \right)\\
--  & \wedge \left( l_{d} \vee \neg l_1 \right)\\
--  & \dots\\
--  & \wedge \left( l_{d} \vee \neg l_k \right)
-- \end{aligned}
-- \]
iffDisjunctionOf  :: Lit a -> [Lit a] -> [[Lit a]]
iffDisjunctionOf disjOfLits lits =
  (-disjOfLits : lits) : map (\l -> [disjOfLits, -l]) lits

iff :: Lit a -> Lit a -> [[Lit a]]
iff l1 l2 =
    [ [  l1, -l2 ]
    , [ -l1,  l2 ]
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
atMostOneOf = zipWithSuccs banPair . nub
  where
    banPair :: Lit a -> Lit a -> [Lit a]
    banPair l1 l2 = [ -l1, -l2 ]

atLeastOneOf :: [Lit a] -> [[Lit a]]
atLeastOneOf lits = [ lits ]

noneOf :: [Lit a] -> [[Lit a]]
noneOf lits = [ [ -l ] | l <- lits ] 

allOf :: [Lit a] -> [[Lit a]]
allOf lits = [ [ l ] | l <- lits ]