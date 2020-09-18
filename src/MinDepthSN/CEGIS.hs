{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}


module MinDepthSN.CEGIS where
import Debug.Trace
import Data.List
import Control.Monad (when)
import SAT.IPASIR
import MinDepthSN.SAT.Synthesis.ConstraintsBZ
import MinDepthSN.SAT.Synthesis.VarsBZ
import MinDepthSN.SAT.CexRun.Constraints
-- import MinDepthSN.SAT.CexRun.Variables
import MinDepthSN.Data.GateOrUnused
import MinDepthSN.Data.Gate (KnownNetType, NetworkType(..))
import MinDepthSN.Data.Size
import MinDepthSN.Data.Value
import MinDepthSN.Data.Window
import Data.Word (Word32)

main :: IO ()
-- main = print $ runSolver (addClauses minimalRepresentative >> solve)
main = print (networkSolution :: Either [Bool] [GateOrUnused 'Standard])

networkSolution :: forall t. KnownNetType t => Either [Bool] [GateOrUnused t]
networkSolution = runSolver $ do

    -- add mandatory initial constraints
    addClauses usage
    addClauses maximalFirstLayer
    --addClauses representativesOfBzIsomorphicEqClasses

    -- possibly add initial counterexample constraints
    let initCexCnt = 0
    let initCexs = take initCexCnt . prioritizeSmallWindows $ inputs
    let (cxData, sortsCexs) = mapAccumR (\(cIdx, cOffset) cx -> ((cIdx + 1, cOffset + fromIntegral (n * (d+1))), sorts cOffset cx)) (0 :: Word32, 0) initCexs
    addClauses (concat sortsCexs)

    -- start cegis
    -- actually synthesizeSortingNetwork should be called here but we lack a cex if initCexCnt = 0
    sat <- solve
    when (not sat) $ error "no network was found initially" -- instead return previous cex i.e. an element from initCexs?
    network <- trueAssigned [ minBound .. maxBound :: GateOrUnused t ]
    validateSortingNetwork cxData network

-- synthesize a network, that also sorts the given input
-- if synthesization succeeds: validate that this new network is a sorting network 
synthesizeSortingNetwork :: KnownNetType t => (Word32, Word32) -> [Bool] -> Solver s (NetworkSynthesis t) (Either [Bool] [GateOrUnused t])
synthesizeSortingNetwork (cexIdx, cexOffset) cexInput = do
    r <- solveCNFs [ sorts cexOffset cexInput ]
    if r then do
        network <- trueAssigned [ minBound .. maxBound ]
        validateSortingNetwork (cexIdx, cexOffset) network
    else pure $ Left cexInput

-- validate that a network is a sorting network by confirming the absence of counterexample runs
-- if validation fails: synthesize a new network that also sorts the counterexample
validateSortingNetwork :: KnownNetType t => (Word32, Word32) -> [GateOrUnused t] -> Solver s (NetworkSynthesis t) (Either [Bool] [GateOrUnused t])
validateSortingNetwork (cexIdx,cexOffset) network = case findCounterexampleRun network of
    Nothing -> pure $ Right network
    Just cexInput -> trace (show cexIdx ++ ": " ++ (concatMap (show . fromEnum) cexInput)) $
        synthesizeSortingNetwork (cexIdx + 1, cexOffset + fromIntegral (n * (d+1))) cexInput

-- find a counterexample run where some input is not sorted
findCounterexampleRun :: KnownNetType t => [GateOrUnused t] -> Maybe [Bool] -- unnecessary KnownNetType constraint?
findCounterexampleRun network = runSolver $ do
    s <- solveCNFs [fixNetwork network, unsortedOutput]
    if s then do
        -- let positions = [ minBound .. maxBound ] :: [CexRun]
        -- vals <- assignments positions
        -- let positionValues = zip vals positions
        counterexampleInput <- assignments inputValues
        pure $ Just
            {-
            $ trace (show network ++ "\n" ++ show positionValues ++ "\n")
            -}
            counterexampleInput
    else pure Nothing
