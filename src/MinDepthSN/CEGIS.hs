module MinDepthSN.CEGIS where

import Data.List
import Data.Bits
import Enumerate ( enumerated )
import SAT.IPASIR
import MinDepthSN.SAT.Synthesis.ConstraintsBZ
import MinDepthSN.SAT.Synthesis.VarsBZ
import MinDepthSN.SAT.CounterExample.Constraints
import MinDepthSN.Data.GateOrUnused
import MinDepthSN.Data.Size
import MinDepthSN.Data.Value
import Numeric.Natural
import Debug.Trace

main :: IO ()
main = print networkSolution

networkSolution :: Either [Bool] [GeneralizedGateOrUnused]
networkSolution = runSolver $ do
    --addCNF representativesOfBzIsomorphicEqClasses
    let initCexCnt = 0
     --(2^n) `div` 2
    network <- findNetwork initCexCnt
    let maybeCex = findCounterExample network
    case maybeCex of
        Nothing -> error $ "no initial cex: " ++ show network
        Just cex -> findSortingNetwork (fromInteger initCexCnt+1) cex

findNetwork :: SortOrder o => Integer -> Solver s (NetworkSynthesis o) [GateOrUnused o]
findNetwork initCexCnt = do
    let initCexs = genericTake initCexCnt . prioritizeSmallWindows $ inputs
    let (_, sortsCexs) = mapAccumL (\cIdx cx -> (cIdx+1, sorts cIdx cx)) 0 initCexs
    r <- solveCNFs $ usage : sortsCexs
    if r
        then trueAssigned enumerated
        else error "no network was found initially"

-- | Prioritize counterexample inputs with a small window size.
-- Disregard sorted inputs as they do not function as counterexamples.
prioritizeSmallWindows ::  [[Bool]] -> [[Bool]]
prioritizeSmallWindows = dropWhile isSorted . sortOn windowSize

-- | An input is sorted iff. its window size is 0.
-- Any input value is either a leading zero or a trailing one.
isSorted :: [Bool] -> Bool
isSorted = (== 0) . windowSize

inputs :: [[Bool]]
inputs = map (\c -> map (testBit c) [0..n-1]) [0..2^n-1 :: Word]

windowSize :: [Bool] -> Int
windowSize input = length input - (leadingZeroes input + trailingOnes input)

leadingZeroes :: [Bool] -> Int 
leadingZeroes = length . takeWhile not

trailingOnes :: [Bool] -> Int
trailingOnes = length . takeWhile id . reverse

--ExceptT Alternative/MonadPlus instance to collect cex for cegis?

-- find a network, that sorts the given input and then look if there is still a counterexample input that is not sorted
findSortingNetwork :: SortOrder o => Natural -> [Bool] -> Solver s (NetworkSynthesis o) (Either [Bool] [GateOrUnused o])
findSortingNetwork cexIdx cex = do
    r <- solveCNFs [ sorts cexIdx cex ]
    if r then do
        network <- trueAssigned enumerated
        {-vals <- assignmentsOfValueVars cexIdx
        let positions = (map fromDIMACS $ range minCounterExample maxCounterExample) :: [Var CounterExample]
        let positionValues = zip vals positions-}
        case findCounterExample {-  trace (show network ++ "\n" ++ show positionValues ++ "\n")-} network of
            Just cex2 -> trace (show cexIdx ++ ": " ++ show cex2) $ findSortingNetwork (cexIdx + 1) cex2
            Nothing -> return $ Right network
    else return $ Left cex

findCounterExample :: SortOrder o => [GateOrUnused o] -> Maybe [Bool]
findCounterExample network = runSolver $ do
    s <- solveCNFs [fixNetwork network, unsortedOutput]
    if s then do
        --vals <- assignmentsOfRange minCounterExample maxCounterExample
        --let positions = (map fromDIMACS $ range minCounterExample maxCounterExample) :: [Var CounterExample]
        --let positionValues = zip vals positions
        counterExampleInput <- assignments inputValues
        return $ Just counterExampleInput
        --return $ Just (counterExampleInput, positionValues)
    else return Nothing





--sorts = undefined
