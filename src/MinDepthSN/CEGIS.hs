{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module MinDepthSN.CEGIS where
import Debug.Trace
import Data.List
import Control.Monad (when)
import SAT.IPASIR
import MinDepthSN.SAT.Synthesis.ConstraintsBZ
import MinDepthSN.SAT.CexRun.Constraints
import MinDepthSN.Vars
import MinDepthSN.Data.Window
import Data.Word (Word32)

import Type.Reflection
import Data.Typeable (eqT)

main :: IO ()
-- main = print $ runSolver (addClauses minimalRepresentative >> solve)
main = print (networkSolution :: Either [Bool] [GateOrUnused 'Standard])


sorts' :: forall t. KnownNetType t => Word32 -> [Bool] -> [[Lit (NetworkSynthesis t)]]
sorts' = case eqT :: Maybe (t :~: 'Standard) of
    Just Refl -> sortsEM
    Nothing   -> case eqT :: Maybe (t :~: 'Generalized) of
        Just Refl -> sorts
        _         -> error "bad"

usageOfOneInUpTo' :: forall t. KnownNetType t => [[Lit (NetworkSynthesis t)]]
usageOfOneInUpTo' = case eqT :: Maybe (t :~: 'Standard) of
    Just Refl -> usageOfOneInUpTo
    Nothing   -> []


networkSolution :: forall t. KnownNetType t => Either [Bool] [GateOrUnused t]
networkSolution = runSolver $ do

    -- add mandatory initial constraints
    addClauses usage
    -- addClauses maximalFirstLayer
    addClauses usageOfOneInUpTo'
    --addClauses representativesOfBzIsomorphicEqClasses

    -- possibly add initial counterexample constraints
    let initCexCnt = 0
    let initCexs = take initCexCnt . prioritizeSmallWindows $ inputs
    let (cxData, sortsCexs) = mapAccumR (\(cIdx, cOffset) cx -> ((cIdx + 1, cOffset + fromIntegral (n * (d+1))), sorts' cOffset cx)) (0 :: Word32, 0) initCexs
    addClauses (concat sortsCexs)

    -- start cegis
    -- actually synthesizeSortingNetwork should be called here but we lack a cex if initCexCnt = 0
    sat <- solve
    when (not sat) $ error "no network was found initially" -- instead return previous cex i.e. an element from initCexs?
    network <- trueAssigned GateOrUnused_ [ minBound .. maxBound ]
    validateSortingNetwork cxData network

-- synthesize a network, that also sorts the given input
-- if synthesization succeeds: validate that this new network is a sorting network 
synthesizeSortingNetwork :: forall s t. KnownNetType t => (Word32, Word32) -> [Bool] -> Solver s (NetworkSynthesis t) (Either [Bool] [GateOrUnused t])
synthesizeSortingNetwork (cexIdx, cexOffset) cexInput = do
    r <- solveCNFs [ sorts' cexOffset cexInput ]
    if r then do
        network <- trueAssigned GateOrUnused_ [ minBound .. maxBound ]
        -- let fromOneInUpTos = [minBound .. maxBound] :: [FromOneInUpTo t]
        -- fromAss <- assignments FromOneInUpTo_ [ minBound .. maxBound ]
        -- let fromValues = zip fromAss fromOneInUpTos
        -- let toOneInUpTos = [minBound .. maxBound] :: [ToOneInUpTo t]
        -- toAss <- assignments ToOneInUpTo_ [ minBound .. maxBound ]
        -- let toValues = zip toAss toOneInUpTos
        -- let positions = [ minBound .. maxBound ] :: [Value]
        -- vals <- assignments (\v -> Value_ v (0::Word32)) positions
        -- let positionValues = zip vals positions
        validateSortingNetwork (cexIdx + 1, cexOffset + fromIntegral (n * (d+1))) 
            {-
            $ trace (
                let sameLayer (GateOrUnused _ _ k) (GateOrUnused _ _ l) = k == l
                    sameLayer _ _ = error ""
                    sameLayerF (_, (FromOneInUpTo _ _ k))(_, (FromOneInUpTo _ _ l)) = k == l
                    sameLayerF _ _ = error ""
                    sameLayerT (_, (ToOneInUpTo _ _ k))(_, (ToOneInUpTo _ _ l)) = k == l
                    sameLayerT _ _ = error ""
                    sameLayer' (_,(Value _ k)) (_,(Value _ l)) = k == l
                    sameLayer' _ _ = error ""
                in (unlines . map show $ groupBy sameLayer network) ++ "\n"
                ++ (unlines . map show $ groupBy sameLayerF fromValues) ++ "\n"
                ++ (unlines . map show $ groupBy sameLayerT toValues) ++ "\n"
                ++ (unlines . map show $ groupBy sameLayer' positionValues) ++ "\n")
            -}
            network
    else pure $ Left cexInput

-- validate that a network is a sorting network by confirming the absence of counterexample runs
-- if validation fails: synthesize a new network that also sorts the counterexample
validateSortingNetwork :: KnownNetType t => (Word32, Word32) -> [GateOrUnused t] -> Solver s (NetworkSynthesis t) (Either [Bool] [GateOrUnused t])
validateSortingNetwork (cexIdx,cexOffset) network = case findCounterexampleRun network of
    Nothing -> pure $ Right network
    Just cexInput -> trace (show cexIdx ++ ": " ++ (concatMap (show . fromEnum) cexInput)) $
        synthesizeSortingNetwork (cexIdx, cexOffset) cexInput

-- find a counterexample run where some input is not sorted
findCounterexampleRun :: KnownNetType t => [GateOrUnused t] -> Maybe [Bool] -- unnecessary KnownNetType constraint?
findCounterexampleRun network = runSolver $ do
    s <- solveCNFs [fixNetwork network, unsortedOutput]
    if s then do
        -- let positions = [ minBound .. maxBound ] :: [CexRun]
        -- vals <- assignments id positions
        -- let positionValues = zip vals positions
        counterexampleInput <- assignments Value_ inputValues
        pure $ Just
            {-
            $ trace (
                let sameLayer (GateOrUnused _ _ k) (GateOrUnused _ _ l) = k == l
                    sameLayer _ _ = error ""
                    sameLayer' (_,(Value _ k)) (_,(Value _ l)) = k == l
                    sameLayer' _ _ = error ""
                in (unlines . map show $ groupBy sameLayer network) ++ "\n"
                ++ (unlines . map show $ groupBy sameLayer' positionValues) ++ "\n")
            -}
            counterexampleInput
    else pure Nothing
