{-# language RankNTypes #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language StandaloneDeriving #-}
{-# language DeriveFunctor #-}
{-# language TypeFamilies #-}

{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}

module SAT.IPASIR
    ( Solver
    , runSolver
    -- * Add clauses and solve
    , addClause
    , addClauses
    , solve
    , solveCNF
    , solveCNFs
    -- * Query assignments
    , isTrueAssigned
    , isFalseAssigned
    , assignments
    , trueAssigned
    , falseAssigned
    -- * Variables
    , AsVar(..)
    , var
    , Var(..)
    -- * Literals
    , lit
    , polarize
    , SAT.IPASIR.negate
    , Lit(..)
    -- * DIMACS encoding
    , Dimacs(..)
    ) where


import Control.Monad (filterM)

import Foreign.C.Types (CInt)

import SAT.IPASIR.Bindings hiding (Solver, runSolver)
import qualified SAT.IPASIR.Bindings as IPASIR
import Control.Monad.Primitive

-- newtype Solver
-- ensure correct state: input/sat/unsat
-- track max var index, ensure valid requests for ipasirVal/ipasirConflict

newtype Solver s v a = Solver { unSolver :: IPASIR.Solver s a }
    deriving (Functor, Applicative, Monad, Semigroup, Monoid)

instance PrimMonad (Solver s v) where
    type PrimState (Solver s v) = s
    primitive = Solver . primitive

runSolver :: Enum v => (forall s. Solver s v a) -> a
runSolver solver = IPASIR.runSolver (unSolver solver)

-- add a 2-literal clause containing both +v and -v where v is the largest var.
-- this will force the solver to allocate all variables?, even if they are not added later.
-- It will also not change the result.
-- runSolver :: (Enum v, Bounded v) => (forall s. Solver s v a) -> a
-- runSolver solver = IPASIR.runSolver (unSolver (addClause [Positive $ Var maxBound, Negative $ Var maxBound] >> solver))

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

negate :: Lit v -> Lit v
negate literal = case literal of
    Positive variable -> Negative variable
    Negative variable -> Positive variable

class Enum v => AsVar v a where
    asVar :: a -> v

lit :: AsVar v a => a -> Lit v
lit = Positive . var

var :: AsVar v a => a -> Var v
var = Var . asVar

-- perhaps a bad name, since another use of the term polarity is:
-- The polarity determines wether a variable is first 
-- assigned true (positive polarity) or false (negative polarity)
-- wouldn't it be ideal to rename this to lit and have unary prefix operators:
-- (-) :: a -> Lit v
-- (+) :: a -> Lit v
polarize :: AsVar v a => Bool -> a -> Lit v
polarize polarity variable = case polarity of
    True  -> Positive $ var variable
    False -> Negative $ var variable

-- | @fromDIMACS 0@ is undefined, otherwise it holds:
--
-- > toDIMACS . fromDIMACS == id
-- > fromDIMACS . toDIMACS == id
class Dimacs a where
    -- | Here @negate@ works on @a@ and @(-)@ works on @CInt@.
    --
    -- > toDIMACS x /= 0
    -- > toDIMACS (negate x) == -toDIMACS x
    toDIMACS :: a -> CInt

    -- | Here @negate@ works on @a@ and @(-)@ works on @CInt@.
    --
    -- > fromDIMACS 0 ~> undefined
    -- > negate (fromDIMACS x) == fromDIMACS (-x)
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
addLiteral = Solver . ipasirAdd . toDIMACS

finalizeClause :: Enum v => Solver s v ()
finalizeClause = Solver $ ipasirAdd 0

addClause :: Enum v => [Lit v] -> Solver s v ()
addClause literals = mapM_ addLiteral literals >> finalizeClause

addClauses :: Enum v => [[Lit v]] -> Solver s v ()
addClauses = mapM_ addClause

solve :: Enum v => Solver s v Bool
solve = Solver ipasirSolve

solveCNF :: Enum v => [[Lit v]] -> Solver s v Bool
solveCNF clauses = addClauses clauses >> solve

-- | Solves a conjunction of cnf formulas.
solveCNFs :: Enum v => [[[Lit v]]] -> Solver s v Bool
solveCNFs cnfs =  mapM_ addClauses cnfs >> solve

isTrueAssigned :: forall s v a. AsVar v a => a -> Solver s v Bool
isTrueAssigned = Solver . ipasirVal . toDIMACS . (var :: a -> Var v)

isFalseAssigned :: AsVar v a =>  a -> Solver s v Bool
isFalseAssigned literal = not <$> isTrueAssigned literal

assignments :: AsVar v a => [a] -> Solver s v [Bool]
assignments = mapM isTrueAssigned

trueAssigned :: AsVar v a => [a] -> Solver s v [a]
trueAssigned = filterM isTrueAssigned

falseAssigned :: AsVar v a => [a] -> Solver s v [a]
falseAssigned = filterM isFalseAssigned
