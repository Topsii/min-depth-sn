{-# language RankNTypes #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language DerivingStrategies #-}
{-# language DeriveFunctor #-}
{-# language TypeFamilies #-}

{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
{-# language LambdaCase #-}

module SAT.IPASIR
    ( Solver
    , runSolver
    -- * Adding clauses
    , addClause
    , addClauses
    -- * Solving
    , SolveResult(..)
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
    , Var(..)
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


data SolveResult = Satisfiable | Unsatisfiable
    deriving stock (Eq,Show)

-- newtype Solver
-- ensure correct state: input/sat/unsat
-- track max var index, ensure valid requests for ipasirVal/ipasirConflict

newtype Solver s v a = Solver { unSolver :: IPASIR.Solver s a }
    deriving newtype (Functor, Applicative, Monad, Semigroup, Monoid)

instance PrimMonad (Solver s v) where
    type PrimState (Solver s v) = s
    primitive = Solver . primitive

runSolver :: Enum v => (forall s. Solver s v a) -> a
runSolver solver = IPASIR.runSolver (unSolver solver)

-- add a 2-literal clause containing both +v and -v where v is the largest var.
-- this will force the solver to allocate all variables?, even if they are not added later.
-- It will also not change the result.
-- runSolver :: (Enum v, Bounded v) => (forall s. Solver s v a) -> a
-- runSolver solver = IPASIR.runSolver (unSolver (addClause [PosLit $ Var maxBound, NegLit $ Var maxBound] >> solver))

newtype Var a = Var { unVar :: a }
    deriving newtype (Eq, Ord, Bounded, Enum)
    deriving stock (Functor)

data Lit a = PosLit a | NegLit a
    deriving stock (Functor, Eq, Ord)

negate :: Lit v -> Lit v
negate = \case
    PosLit variable -> NegLit variable
    NegLit variable -> PosLit variable

-- perhaps a bad name, since another use of the term polarity is:
-- The polarity determines wether a variable is first 
-- assigned true (positive polarity) or false (negative polarity)
-- wouldn't it be ideal to rename this to lit and have unary prefix operators:
-- (-) :: a -> Lit v
-- (+) :: a -> Lit v
polarize :: Bool -> v -> Lit v
polarize = \case
    True  -> PosLit
    False -> NegLit

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
    -- fromDIMACS :: CInt -> a


instance Show a => Show (Var a) where
    show (Var x) = 'v' : show x

instance Show a => Show (Lit a) where
    showsPrec p = showParen (p >= 11) . \case
        PosLit variable -> showChar '+' . showsPrec 11 variable
        NegLit variable -> showChar '-' . showsPrec 11 variable

-- ideally use toIntegerSized from Data.Bits here to ensure the conversion
-- between CInt and Int works as intended?
-- Also ideally make sure we can add 1?
instance Enum a => Dimacs (Var a) where
    toDIMACS variable = fromIntegral (fromEnum variable) + 1
    -- fromDIMACS integer = toEnum (fromIntegral integer - 1)

instance Dimacs a => Dimacs (Lit a) where
    toDIMACS = \case
        PosLit variable ->   toDIMACS variable
        NegLit variable -> - toDIMACS variable
    -- fromDIMACS integer
    --     | integer < 0 = NegLit $ fromDIMACS (- integer)
    --     | integer > 0 = PosLit $ fromDIMACS    integer
    --     | otherwise = error "fromDIMACS: 0 represents no literal"

addLiteral :: Dimacs v => Lit v -> Solver s v ()
addLiteral = Solver . ipasirAdd . toDIMACS

finalizeClause :: Dimacs v => Solver s v ()
finalizeClause = Solver $ ipasirAdd 0

addClause :: Dimacs v => [Lit v] -> Solver s v ()
addClause literals = mapM_ addLiteral literals >> finalizeClause

addClauses :: Dimacs v => [[Lit v]] -> Solver s v ()
addClauses = mapM_ addClause

solve :: Dimacs v => Solver s v SolveResult
solve = toSolveResult <$> Solver ipasirSolve
  where
    toSolveResult :: Bool -> SolveResult
    toSolveResult r = if r then Satisfiable else Unsatisfiable


solveCNF :: Dimacs v => [[Lit v]] -> Solver s v SolveResult
solveCNF clauses = addClauses clauses >> solve

-- | Solves a conjunction of cnf formulas.
solveCNFs :: Dimacs v => [[[Lit v]]] -> Solver s v SolveResult
solveCNFs cnfs =  mapM_ addClauses cnfs >> solve

isTrueAssigned :: Dimacs v => (a -> v) -> a -> Solver s v Bool
isTrueAssigned var = Solver . ipasirVal . toDIMACS . var

isFalseAssigned :: Dimacs v => (a -> v) -> a -> Solver s v Bool
isFalseAssigned var x = not <$> (isTrueAssigned var) x

assignments :: Dimacs v => (a -> v) -> [a] -> Solver s v [Bool]
assignments = mapM . isTrueAssigned 

trueAssigned :: Dimacs v => (a -> v) -> [a] -> Solver s v [a]
trueAssigned = filterM . isTrueAssigned

falseAssigned :: Dimacs v => (a -> v) -> [a] -> Solver s v [a]
falseAssigned = filterM . isFalseAssigned 
