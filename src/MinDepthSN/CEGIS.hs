
{-# LANGUAGE FlexibleContexts #-}

module MinDepthSN.CEGIS where
import Debug.Trace
import Data.List
import Data.Bits
import SAT.IPASIR
import MinDepthSN.SAT.Synthesis.ConstraintsBZ
import MinDepthSN.SAT.Synthesis.VarsBZ
import MinDepthSN.SAT.Counterexample.Constraints
-- import MinDepthSN.SAT.Counterexample.Variables
-- import MinDepthSN.SAT.Synthesis.ConstraintsHaslop
import MinDepthSN.Data.GateOrUnused
import MinDepthSN.Data.Size
import MinDepthSN.Data.Value
import Numeric.Natural

main :: IO ()
-- main = print $ runSolver (addClauses minimalRepresentative >> solve)
main = print networkSolution

networkSolution :: Either [Bool] [GateOrUnused]
networkSolution = runSolver $ do
    --addCNF representativesOfBzIsomorphicEqClasses
    let initCexCnt = 0
     --(2^n) `div` 2
    network <- findNetwork initCexCnt
    let maybeCex = findCounterexample network
    case maybeCex of
        Nothing -> error $ "no initial cex: " ++ show network
        Just cex -> findSortingNetwork (fromInteger initCexCnt) cex

-- findFirstNetwork :: Integer -> ExceptRT (Maybe [GateOrUnused o]) (Solver s NetworkSynthesis) [Bool]
-- findFirstNetwork initCexCnt = do
--     network <- findNetwork initCexCnt
--     let maybeCex = findCounterexample network
--     case maybeCex of
--         Nothing -> error $ "no initial cex: " ++ show network
--         Just cex -> findSortingNetwork (fromInteger initCexCnt+1) cex

findNetwork :: Integer -> Solver s NetworkSynthesis [GateOrUnused]
findNetwork initCexCnt = do
    let initCexs = genericTake initCexCnt . prioritizeSmallWindows $ inputs
    let (_, sortsCexs) = mapAccumL (\cIdx cx -> (cIdx+1, sorts cIdx cx)) 0 initCexs
    r <- solveCNFs $ [usage, maximalFirstLayer] ++ sortsCexs
    if r
        then trueAssigned [ minBound .. maxBound ]
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
findSortingNetwork :: Natural -> [Bool] -> Solver s NetworkSynthesis (Either [Bool] [GateOrUnused])
findSortingNetwork cexIdx cex = do
    r <- solveCNFs [ sorts cexIdx cex ]
    if r then do
        network <- trueAssigned [ minBound .. maxBound ]
        -- vals <- assignments [Value_ cexIdx minBound .. Value_ cexIdx maxBound]
        -- let positions = [ minBound .. maxBound ] :: [Counterexample]
        -- let positionValues = zip vals positions
        case findCounterexample {- $ trace (show network ++ "\n" ++ show positionValues ++ "\n") -} network of
            Just cex2 -> trace (show cexIdx ++ ": " ++ (concatMap (show . fromEnum) cex2)) $ findSortingNetwork (cexIdx + 1) cex2
            Nothing -> return $ Right network
    else return $ Left cex

findCounterexample :: [GateOrUnused] -> Maybe [Bool]
findCounterexample network = runSolver $ do
    s <- solveCNFs [fixNetwork network, unsortedOutput]
    if s then do
        -- let positions = [ minBound .. maxBound ] :: [Counterexample]
        -- vals <- assignments positions
        -- let positionValues = zip vals positions
        counterexampleInput <- assignments inputValues
        return $ Just {- $ trace (show positionValues)-}  counterexampleInput
    else return Nothing
