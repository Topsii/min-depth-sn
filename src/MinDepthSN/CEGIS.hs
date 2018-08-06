module MinDepthSN.CEGIS where

import SAT.IPASIR.EnumVars
import MinDepthSN.SAT.Synthesis.Constraints
import MinDepthSN.SAT.Synthesis.Variables
import MinDepthSN.SAT.CounterExample.Constraints
import MinDepthSN.SAT.CounterExample.Variables
import MinDepthSN.Data.GateOrUnused (GateOrUnused)
import Numeric.Natural
-- import Debug.Trace

main :: IO ()
main = print $ runSolver $ do
        network <- findNetwork
        let maybeCex = findCounterExample network
        case maybeCex of
            Nothing -> error "no initial cex"
            Just cex -> findSortingNetwork 0 cex

findNetwork :: Solver s NetworkSynthesis [GateOrUnused]
findNetwork = do
    r <- solve [ usageOnce, unused, maximalFirstLayer ]
    if r 
        then trueAssignmentsOfGateOrUnusedVars
        else error "no network was found initially"

-- find a network, that sorts the given input and then look if there is still a counterexample input that is not sorted
findSortingNetwork :: Natural -> [Bool] -> Solver s NetworkSynthesis (Either [Bool] [GateOrUnused])
findSortingNetwork cexIdx cex = do
    r <- solve [ sorts cexIdx cex ]
    if r then do
        network <- trueAssignmentsOfGateOrUnusedVars
        {-vals <- assignmentsOfRange (minValueVar cexIdx) (maxValueVar cexIdx)
        let positions = (map fromDIMACS $ range minCounterExample maxCounterExample) :: [Var CounterExample]
        let positionValues = zip vals positions-}
        case findCounterExample $ {-trace (show network ++ "\n" ++ show positionValues ++ "\n") -}network of
            Just cex2 -> {-trace (show cex2 ++ "\n") $-} findSortingNetwork (cexIdx + 1) cex2
            Nothing -> return $ Right network
    else return $ Left cex

findCounterExample :: [GateOrUnused] -> Maybe [Bool]
findCounterExample network = runSolver $ do
    s <- solve [fixNetwork network, unsorted]
    if s then do
        --vals <- assignmentsOfRange minCounterExample maxCounterExample
        --let positions = (map fromDIMACS $ range minCounterExample maxCounterExample) :: [Var CounterExample]
        --let positionValues = zip vals positions
        counterExampleInput <- assignmentsOfRange firstInputValueVar lastInputValueVar
        return $ Just counterExampleInput
        --return $ Just (counterExampleInput, positionValues)
    else return Nothing





--sorts = undefined
