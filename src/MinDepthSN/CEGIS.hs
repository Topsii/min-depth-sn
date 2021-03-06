{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module MinDepthSN.CEGIS (main,networkSolution) where
import Debug.Trace
import Data.List
import SAT.IPASIR
import MinDepthSN.SAT.Synthesis.ConstraintsBZ
-- import MinDepthSN.SAT.Synthesis.End
import MinDepthSN.SAT.CexRun.Constraints
import MinDepthSN.Vars

import MinDepthSN.PrefixCost
import MinDepthSN.Data.Window
import Data.Word (Word32)

import Type.Reflection
import Data.Typeable (eqT)
import MinDepthSN.SAT.Synthesis.Greenfilter3rdLayer



main :: IO ()
-- main = print $ runSolver (addClauses minimalRepresentative >> solve)
-- main = do
--     putStrLn $ "n: " ++ show n
--     putStrLn $ "d: " ++ show d
--     -- print networkSolution -- :: Either [Bool] [GateOrUnused 'Standard])
--     print net

main = do
    -- putStrLn "greenfilterOpt32Inputs"
    -- print greenfilterOpt32Inputs
    putStrLn "greenfilterOpt32InputsRev"
    print greenfilterOpt32InputsRev
    -- putStrLn "greenfilterInputsWinSizeDistr"
    -- print $ greenfilterInputsWinSizeDistr
    -- putStrLn "*> windowSizeDistribution All greenfilter32_opt"
    -- print $ windowSizeDistribution All greenfilter32_opt


sorts' :: forall t. KnownNetType t => Word32 -> [Bool] -> [[Lit (NetworkSynthesis t)]]
sorts' cexOffset cex = case eqT :: Maybe (t :~: 'Standard) of
    Just Refl -> sortsEM cexOffset cex
    Nothing   -> case eqT :: Maybe (t :~: 'Generalized) of
        Just Refl -> sorts cexOffset cex
        _         -> error "bad"

-- isAcrossMultipleLayers :: KnownNetType t => Lit (NetworkSynthesis t) -> Bool
-- isAcrossMultipleLayers ns = case ns of
--     PosLit (SortedRel (Value _i k) (Value _j l)) -> abs (fromEnum k - fromEnum l) > 1
--     NegLit (SortedRel (Value _i k) (Value _j l)) -> abs (fromEnum k - fromEnum l) > 1
--     _                                            -> False

networkSolution :: forall t. (KnownNetType t, t ~ 'Standard) => Either [Bool] [GateOrUnused t]
networkSolution = runSolver $ do

    -- add mandatory initial constraints
    addClauses usage
    -- addClauses amoMinMaxRec
    -- addClauses amoMinMax
    addClauses defMinMaxViaGates

    -- addClauses $ map ((: []) . PosLit . gateOrUnused_) x
    -- addClauses prefix
    -- addClauses greenfilterPrefix
    addClauses greenfilterPrefix12
    -- addClauses greenfilterPrefixMod3

    -- addClauses maximalFirstLayer
    -- addClauses saturatedTwoLayerPrefix
    
    -- addClauses $ filter (all (not . isAcrossMultipleLayers)) propagateSR
    -- addClauses banRedundantGates
    -- addClauses saturatedPrefixes

    -- addClauses toBetweenBeforeConstr
    -- addClauses viaWrongTwistConstr
    -- addClauses $ breakParallelGates Earliest
    -- addClauses breakInpTwists

    -- addClauses lastConstr

    -- possibly add initial counterexample constraints
    let initCexCnt = 2700
    let initCexs = take initCexCnt . prioritizeSmallWindows $ inputs
    let (cxData, sortsCexs) = mapAccumR (\(cIdx, cOffset) cx -> ((cIdx + 1, cOffset + fromIntegral (n * (d+1))), sorts' cOffset cx)) (0 :: Word32, 0) initCexs
    addClauses $ concat sortsCexs

    -- start cegis
    -- actually synthesizeSortingNetwork should be called here but we lack a cex if initCexCnt = 0
    sat <- solve
    case sat of
        Unsatisfiable -> error "no network was found initially" -- instead return previous cex i.e. an element from initCexs?
        Satisfiable   -> do
            network <- trueAssigned gateOrUnused_ [ minBound .. maxBound ]
            validateSortingNetwork cxData network

-- synthesize a network, that also sorts the given input
-- if synthesization succeeds: validate that this new network is a sorting network 
synthesizeSortingNetwork :: forall s t. KnownNetType t => (Word32, Word32) -> [Bool] -> Solver s (NetworkSynthesis t) (Either [Bool] [GateOrUnused t])
synthesizeSortingNetwork (cexIdx, cexOffset) cexInput = do
    r <- solveCNFs [ sorts' cexOffset cexInput ]
    case r of
        Unsatisfiable -> pure $ Left cexInput
        Satisfiable   -> do
            network <- trueAssigned gateOrUnused_ [ minBound .. maxBound ]
            -- let fromOneInUpTos = [minBound .. maxBound] :: [FromOneInUpTo t]
            -- fromAss <- assignments fromOneInUpTo_ [ minBound .. maxBound ]
            -- let fromValues = zip fromAss fromOneInUpTos
            -- let toOneInUpTos = [minBound .. maxBound] :: [ToOneInUpTo t]
            -- toAss <- assignments toOneInUpTo_ [ minBound .. maxBound ]
            -- let toValues = zip toAss toOneInUpTos
            -- let positions = [ minBound .. maxBound ] :: [Value]
            -- vals <- assignments (\v -> value_ v (0::Word32)) positions
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

-- validate that a network is a sorting network by confirming the absence of counterexample runs
-- if validation fails: synthesize a new network that also sorts the counterexample
validateSortingNetwork :: KnownNetType t => (Word32, Word32) -> [GateOrUnused t] -> Solver s (NetworkSynthesis t) (Either [Bool] [GateOrUnused t])
validateSortingNetwork (cexIdx,cexOffset) network = case findCounterexampleRun network of
    Nothing -> pure $ Right network
    Just cexInput -> trace (show cexIdx ++ ": " ++ concatMap (show . fromEnum) cexInput) $
        synthesizeSortingNetwork (cexIdx, cexOffset) cexInput

