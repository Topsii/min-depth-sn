module SAT.IPASIR.DimacsSolver
    ( module SAT.IPASIR.Bindings
    , module SAT.IPASIR.DimacsSolver
    ) where

import Control.Monad (filterM)

import Foreign.C.Types (CInt)
import SAT.IPASIR.Bindings

finalizeClause :: Solver s ()
finalizeClause = ipasirAdd 0

addLiteral :: CInt -> Solver s ()
addLiteral = ipasirAdd

addClause :: [CInt] -> Solver s ()
addClause literals = mapM_ addLiteral literals >> finalizeClause

addClauses :: [[CInt]] -> Solver s ()
addClauses = mapM_ addClause

solve :: Solver s Bool
solve = ipasirSolve

solveCNF :: [[CInt]] -> Solver s Bool
solveCNF clauses = addClauses clauses >> solve

-- todo: keep track of the maximum variable index to offer an assignments function for all variables

isTrueAssigned :: CInt -> Solver s Bool
isTrueAssigned = ipasirVal

isFalseAssigned :: CInt -> Solver s Bool
isFalseAssigned lit = not <$> isTrueAssigned lit

assignments :: [CInt] -> Solver s [Bool]
assignments = mapM isTrueAssigned

trueAssigned :: [CInt] -> Solver s [CInt]
trueAssigned = filterM isTrueAssigned

falseAssigned ::  [CInt] -> Solver s [CInt]
falseAssigned = filterM isFalseAssigned