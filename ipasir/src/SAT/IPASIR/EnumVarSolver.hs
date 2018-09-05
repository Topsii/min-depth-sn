{-# language RankNTypes #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language StandaloneDeriving #-}
{-# language DeriveFunctor #-}

{-# language MultiParamTypeClasses #-}
{-# language InstanceSigs #-}
{-# language ScopedTypeVariables #-}

-- rename module to simply 'Solver'? or better make it SAT.IPASIR
module SAT.IPASIR.EnumVarSolver where

import Control.Monad (filterM)
import Foreign.C.Types (CInt)

import qualified SAT.IPASIR.DimacsSolver as DIMACS

-- newtype Solver
-- ensure correct state: input/sat/unsat
-- ensure enum type is always the same: Var a <-> Lit a <-> Solver s a ()
-- track max var index, ensure valid requests for ipasirVal/ipasirConflict

newtype Solver s v a = Solver { unSolver :: DIMACS.Solver s a }
    deriving (Functor, Applicative, Monad, Semigroup, Monoid)


runSolver :: Enum v => (forall s. Solver s v a) -> a
runSolver solver = DIMACS.runSolver (unSolver solver)

-- add a 2-literal clause containing both polarities of max var.
-- this will force the solver to allocate all variables?, even if they are not added later.
-- It will also not change the result.
-- runSolver :: (Enum v, Bounded v) => (forall s. Solver s v a) -> a
-- runSolver solver = DIMACS.runSolver (unSolver (addClause [Positive $ Var maxBound, Negative $ Var maxBound] >> solver))

newtype Var a = Var { unVar :: a }
    deriving (Functor)

deriving instance Eq a => Eq (Var a)
deriving instance Ord a => Ord (Var a)
deriving instance Bounded a => Bounded (Var a)
deriving instance Enum a => Enum (Var a)

data Lit a = Positive (Var a) | Negative (Var a)
    deriving (Functor)

deriving instance Eq a => Eq (Lit a)
deriving instance Ord a => Ord (Lit a)

-- todo remove lens dependency on ipasir
class Enum v => AsVar v a where
    v :: a -> v

    -- v = NetworkSynthesis
    -- a = Gate (here it's Lit a, since Gate is in another pkg)
    -- Lit a ~ Gate o

lit :: AsVar v a => a -> Lit v
lit = Positive . var

var :: AsVar v a => a -> Var v
var = Var . v

polarize :: Bool -> Var x -> Lit x
polarize polarity variable = case polarity of
    True  -> Positive variable
    False -> Negative variable

negate :: Lit a -> Lit a
negate literal = case literal of
    Positive variable -> Negative variable
    Negative variable -> Positive variable

class Dimacs a where
    -- |
    -- toDimacs x /= 0
    -- toDimacs (neg x) == -toDimacs x
    toDIMACS :: a -> CInt

    -- |
    -- fromDimacs 0  undefined
    -- neg (fromDimacs x) == fromDimacs (-x)
    fromDIMACS :: CInt -> a


instance Show a => Show (Var a) where
    show (Var x) = 'v' : show x

instance Show a => Show (Lit a) where
    show literal = case literal of
        Positive variable -> '+' : show variable
        Negative variable -> '-' : show variable

instance Enum a => Dimacs (Var a) where
    toDIMACS variable = fromIntegral (fromEnum variable) + 1
    fromDIMACS integer = toEnum (fromIntegral integer - 1)

instance Enum a => Dimacs (Lit a) where
    toDIMACS literal = case literal of
        Positive variable ->   toDIMACS variable
        Negative variable -> - toDIMACS variable
    fromDIMACS integer
        | integer < 0 = Negative $ fromDIMACS (- integer)
        | integer > 0 = Positive $ fromDIMACS    integer
        | otherwise = error "fromDIMACS: 0 represents no literal"

    
addLiteral :: Enum v => Lit v -> Solver s v ()
addLiteral = Solver . DIMACS.addLiteral . toDIMACS

addClause :: Enum v => [Lit v] -> Solver s v ()
addClause literals = mapM_ addLiteral literals >> Solver DIMACS.finalizeClause

addClauses :: Enum v => [[Lit v]] -> Solver s v ()
addClauses = mapM_ addClause

-- | Solves a conjunction of cnf formulas.
solve :: Enum v => [[[Lit v]]] -> Solver s v Bool
solve cnfs = mapM_ addClauses cnfs >> Solver DIMACS.ipasirSolve

isTrueAssigned :: forall s v a. AsVar v a => a -> Solver s v Bool
isTrueAssigned = Solver . DIMACS.isTrueAssigned . toDIMACS . (var :: a -> Var v)

isFalseAssigned :: AsVar v a =>  a -> Solver s v Bool
isFalseAssigned literal = not <$> isTrueAssigned literal

assignments :: AsVar v a => [a] -> Solver s v [Bool]
assignments = mapM isTrueAssigned

trueAssigned :: AsVar v a => [a] -> Solver s v [a]
trueAssigned = filterM isTrueAssigned

falseAssigned :: AsVar v a => [a] -> Solver s v [a]
falseAssigned = filterM isFalseAssigned

-- -- todo expect a instead of Var v
-- assignmentsOfRange :: Enum v => Var v -> Var v ->  Solver s v [Bool]
-- assignmentsOfRange from to = Solver . DIMACS.assignments $ range from to

-- -- todo expect a instead of Var v
-- trueAssignedOfRange :: Enum v => Var v -> Var v ->  Solver s v [Var v]
-- trueAssignedOfRange from to = Solver $ map fromDIMACS <$> DIMACS.trueAssigned (range from to)

-- -- todo expect a instead of Var v
-- falseAssignedOfRange :: Enum v => Var v -> Var v ->  Solver s v [Var v]
-- falseAssignedOfRange from to = Solver $ map fromDIMACS <$> DIMACS.falseAssigned (range from to)

-- -- what if from minBound to maxBound is queried, but maxBound was never added as a literal?
-- range :: Enum a => Var a -> Var a -> [CInt]
-- range from to = [ toDIMACS from .. toDIMACS to ]

--replace args by min-/maxBound and use TypeApplications when calling?
--all :: Enum a => [Int]
--all = range minBound maxBound
