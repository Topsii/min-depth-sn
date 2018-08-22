module MinDepthSN.CEGIS where

import SAT.IPASIR.EnumVars
import MinDepthSN.SAT.Synthesis.ConstraintsBZ
import MinDepthSN.SAT.Synthesis.VarsBZ
import MinDepthSN.SAT.CounterExample.Constraints
import MinDepthSN.SAT.CounterExample.Variables
import MinDepthSN.Data.GateOrUnused
import Numeric.Natural
-- import Debug.Trace

main :: IO ()
main = print networkSolution

networkSolution :: Either [Bool] [StandardGateOrUnused]
networkSolution = runSolver $ do
    --addCNF representativesOfBzIsomorphicEqClasses
    network <- findNetwork
    let maybeCex = findCounterExample network
    case maybeCex of
        Nothing -> error "no initial cex"
        Just cex -> findSortingNetwork 0 cex

findNetwork :: SortOrder o => Solver s (NetworkSynthesis o) [GateOrUnused o]
findNetwork = do
    r <- solve [ usage ]
    if r
        then trueAssignedGateOrUnusedVars
        else error "no network was found initially"

-- find a network, that sorts the given input and then look if there is still a counterexample input that is not sorted
findSortingNetwork :: SortOrder o => Natural -> [Bool] -> Solver s (NetworkSynthesis o) (Either [Bool] [GateOrUnused o])
findSortingNetwork cexIdx cex = do
    r <- solve [ sorts cexIdx cex ]
    if r then do
        network <- trueAssignedGateOrUnusedVars
        {-vals <- assignmentsOfValueVars cexIdx
        let positions = (map fromDIMACS $ range minCounterExample maxCounterExample) :: [Var CounterExample]
        let positionValues = zip vals positions-}
        case findCounterExample {-$ trace (show network ++ "\n" ++ show positionValues ++ "\n")-} network of
            Just cex2 -> {-trace (show cexIdx ++ ": " ++ show cex2) $-} findSortingNetwork (cexIdx + 1) cex2
            Nothing -> return $ Right network
    else return $ Left cex

findCounterExample :: SortOrder o => [GateOrUnused o] -> Maybe [Bool]
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
