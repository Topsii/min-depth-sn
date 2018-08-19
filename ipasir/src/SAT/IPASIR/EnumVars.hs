{-# language RankNTypes #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language StandaloneDeriving #-}
{-# language DeriveFunctor #-}

module SAT.IPASIR.EnumVars where

import Control.Monad ((>=>))
import Foreign.C.Types (CInt)

import qualified SAT.IPASIR.IntegralLits as DIMACS
import SAT.IPASIR.IntegralLits (ipasirAdd, ipasirSolve, finalizeClause, trueAssignedVarsOf, falseAssignedVarsOf, assignmentsOf)

-- newtype Solver
-- ensure correct state: input/sat/unsat
-- ensure enum type is always the same: Var a <-> Lit a <-> Solver s a ()
-- track max var index, ensure valid requests for ipasirVal/ipasirConflict

newtype Solver s v a = Solver { unSolver :: DIMACS.Solver s a }
    deriving (Functor, Applicative, Monad, Semigroup, Monoid)


runSolver :: Enum v => (forall s. Solver s v a) -> a
runSolver solver = DIMACS.runSolver (unSolver solver)

-- add a 2-literal clause containing both polarities of max var.asTypeOf
-- this will force the solver to allocate all variables?, even if they are not added later.
-- It will also not change the result.
-- runSolver :: (Enum v, Bounded v) => (forall s. Solver s v a) -> a
-- runSolver solver = DIMACS.runSolver (unSolver (addClause [Pos $ Var maxBound, Neg $ Var maxBound] >> solver))

newtype Var a = Var { unVar :: a }
    deriving (Functor)

deriving instance Eq a => Eq (Var a)
deriving instance Ord a => Ord (Var a)
deriving instance Bounded a => Bounded (Var a)
deriving instance Enum a => Enum (Var a)

data Lit a = Pos (Var a) | Neg (Var a)
    deriving (Functor)

deriving instance Eq a => Eq (Lit a)
deriving instance Ord a => Ord (Lit a)


polarize :: Bool -> Var x -> Lit x
polarize polarity var = case polarity of
    True  -> Pos var
    False -> Neg var

negate :: Lit a -> Lit a
negate lit = case lit of
    Pos x -> Neg x
    Neg x -> Pos x

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
    show lit = case lit of
        Pos v -> '+' : show v
        Neg v -> '-' : show v

instance Enum a => Dimacs (Var a) where
    toDIMACS var = fromIntegral (fromEnum var) + 1
    fromDIMACS int = toEnum (fromIntegral int - 1)

instance Enum a => Dimacs (Lit a) where
    toDIMACS lit = case lit of
        Pos var ->  toDIMACS var
        Neg var -> -toDIMACS var
    fromDIMACS int
        | int < 0 = Neg $ fromDIMACS (-int)
        | int > 0 = Pos $ fromDIMACS   int
        | otherwise = error "fromDIMACS: 0 represents no literal"

addLit :: Enum v => Lit v -> Solver s v ()
addLit = Solver . ipasirAdd . toDIMACS

addClause :: Enum v => [Lit v] -> Solver s v ()
addClause = mapM_ addLit >=> const (Solver finalizeClause)

addCNF :: Enum v => [[Lit v]] -> Solver s v ()
addCNF = mapM_ addClause

-- | Solves a conjunction of cnf formulas.
solve :: Enum v => [[[Lit v]]] -> Solver s v Bool
solve cnfs = mapM_ addCNF cnfs >> Solver ipasirSolve

assignmentsOfRange :: Enum v => Var v -> Var v ->  Solver s v [Bool]
assignmentsOfRange from to = Solver $ assignmentsOf $ range from to

trueAssignedVarsOfRange :: Enum v => Var v -> Var v ->  Solver s v [Var v]
trueAssignedVarsOfRange from to = Solver $ map fromDIMACS <$> trueAssignedVarsOf (range from to)

falseAssignedVarsOfRange :: Enum v => Var v -> Var v ->  Solver s v [Var v]
falseAssignedVarsOfRange from to = Solver $ map fromDIMACS <$> falseAssignedVarsOf (range from to)

-- what if from minBound to maxBound is queried, but maxBound was never added as a literal?
range :: Enum a => Var a -> Var a -> [CInt]
range from to = [toDIMACS from .. toDIMACS to]

--replace args by min-/maxBound and use TypeApplications when calling?
--all :: Enum a => [Int]
--all = undefined --range minBound maxBound
