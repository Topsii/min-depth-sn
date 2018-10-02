module MinDepthSN.CEGIS where

import Data.List
import Data.Bits
import Enumerate ( enumerated )
import SAT.IPASIR
import MinDepthSN.SAT.Synthesis.ConstraintsBZ
import MinDepthSN.SAT.Synthesis.VarsBZ
import MinDepthSN.SAT.CounterExample.Constraints
-- import MinDepthSN.SAT.CounterExample.Variables
import MinDepthSN.SAT.Synthesis.ConstraintsHaslop
import MinDepthSN.Data.GateOrUnused
import MinDepthSN.Data.Size
import MinDepthSN.Data.Value
import Numeric.Natural
-- import Data.EitherR
-- import Control.Applicative
-- import Data.Functor.Identity

-- -- newtype CEGIS cex syn = MkCegis (Solver s syn (Either cex syn))

-- newtype Cegis  s1 o f cex syn = Cegis  (ExceptRT (f syn) (Solver s1 o) cex) -- Monoid (f syn), Alternative f
-- newtype Cegis' s2 o g cex syn = Cegis' (ExceptRT (g cex) (Solver s2 o) syn)  -- Monoid (g cex), Alternative g
-- newtype CegisMinDepthSN  s o = CegisMinDepthSN  (ExceptRT (Maybe [GateOrUnused o]) (NetworkSolver s o)  [Bool])
-- newtype CegisMinDepthSN' s o = CegisMinDepthSN' (ExceptRT [[Bool]]                 (CexInputSolver s o) [GateOrUnused o])
-- type NetworkSolver s o = Solver s (NetworkSynthesis o)
-- type CexInputSolver s = Solver s CounterExample

-- cegis' :: Monad m => (cex -> cexsAccum -> m (cexsAccum, Maybe syn)) -> (syn -> Maybe cex) -> cexsAccum -> m (Either cexsAccum syn)
-- unfoldrM' (monad-loops)

-- cegis :: Alternative f => (a -> Either a b) -> (b -> Either b a) -> f (Either a b)
-- cegis synthesize findCex =
--     runIdentity $ cegisM (Identity . synthesize) (Identity . findCex)

-- cegisM :: (Monad m, Alternative f) => (a -> m (Either a b)) -> (b -> m (Either b a)) -> m a -> m (f (Either a b))
-- cegisM synthesize findCex init = init >> synthesize

main :: IO ()
-- main = print $ runSolver (addClauses minimalRepresentative >> solve)
main = print networkSolution

networkSolution :: Either [Bool] [StandardGateOrUnused]
networkSolution = runSolver $ do
    --addCNF representativesOfBzIsomorphicEqClasses
    let initCexCnt = 0
     --(2^n) `div` 2
    network <- findNetwork initCexCnt
    let maybeCex = findCounterExample network
    case maybeCex of
        Nothing -> error $ "no initial cex: " ++ show network
        Just cex -> findSortingNetwork (fromInteger initCexCnt+1) cex

-- findFirstNetwork :: SortOrder o => Integer -> ExceptRT (Maybe [GateOrUnused o]) (Solver s (NetworkSynthesis o)) [Bool]
-- findFirstNetwork initCexCnt = do
--     network <- findNetwork initCexCnt
--     let maybeCex = findCounterExample network
--     case maybeCex of
--         Nothing -> error $ "no initial cex: " ++ show network
--         Just cex -> findSortingNetwork (fromInteger initCexCnt+1) cex

findNetwork :: SortOrder o => Integer -> Solver s (NetworkSynthesis o) [GateOrUnused o]
findNetwork initCexCnt = do
    let initCexs = genericTake initCexCnt . prioritizeSmallWindows $ inputs
    let (_, sortsCexs) = mapAccumL (\cIdx cx -> (cIdx+1, sorts cIdx cx)) 0 initCexs
    r <- solveCNFs $ [usage, maximalFirstLayer] ++ sortsCexs
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
            Just cex2 -> {-trace (show cexIdx ++ ": " ++ show cex2) $-} findSortingNetwork (cexIdx + 1) cex2
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
