module SAT.IPASIR.IntegralLits
    ( module SAT.IPASIR.Bindings
    , module SAT.IPASIR.IntegralLits
    ) where

import Control.Monad (filterM, (>=>))
import Control.Kleislify ((=>^))

import Foreign.C.Types (CInt)
import SAT.IPASIR.Bindings


finalizeClause :: Solver s ()
finalizeClause = ipasirAdd 0

addClause :: [CInt] -> Solver s ()
addClause = mapM_ ipasirAdd >=> const finalizeClause

addCNF :: [[CInt]] -> Solver s ()
addCNF = mapM_ addClause

solve :: [[CInt]] -> Solver s Bool
solve clauses = addCNF clauses >> ipasirSolve

-- todo: keep track of the maximum variable index to offer an assignments function for all variables

assignmentsOf :: [CInt] -> Solver s [Bool]
assignmentsOf = mapM ipasirVal

isTrue :: CInt -> Solver s Bool
isTrue = ipasirVal

isFalse :: CInt -> Solver s Bool
isFalse = ipasirVal =>^ not

trueAssignedVarsOf :: [CInt] -> Solver s [CInt]
trueAssignedVarsOf = filterM isTrue

falseAssignedVarsOf ::  [CInt] -> Solver s [CInt]
falseAssignedVarsOf = filterM isFalse