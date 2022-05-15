{-# OPTIONS_GHC -Wwarn #-}
{-# language ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# language DerivingStrategies #-}

module MinDepthSN.PrefixCost where

import MinDepthSN.Vars
import MinDepthSN.Data.Window


import Data.Array.Unboxed
import Data.Bits
import Data.List 
import Data.Function (on)
import Data.Ord
import Data.Containers.ListUtils ( nubOrd )
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe (fromJust)

nubSort :: Ord a => [a] -> [a]
nubSort = Set.toAscList . Set.fromList

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = Map.toList (Map.fromListWith (+) [(x, 1) | x <- xs])

windowCost :: [Bool] -> Int
windowCost input = (size * (size - 1)) `div` 2
  where
    size = windowSize input

inputs' :: [UArray Channel Bool]
inputs' = map (listArray (minBound, maxBound)) inputs


runNetwork :: forall t. KnownNetType t => [Gate t] -> UArray Channel Bool -> UArray Channel Bool
runNetwork net input = foldl' applyLayer input (groupBy ((==) `on` layer) $ sortOn layer net)
  where
    layer :: Gate t -> Layer
    layer (Gate _i _j k) = k
    applyLayer :: UArray Channel Bool -> [Gate t] -> UArray Channel Bool
    applyLayer x gates = x // concatMap applyGate gates
     where
      applyGate :: Gate t -> [(Channel, Bool)]
      applyGate (Gate i j _k) = [(i, min xi xj), (j, max xi xj)]
        where
          xi, xj :: Bool
          xi = x ! i
          xj = x ! j

prefix18_1 :: [Gate 'Standard]
prefix18_1 = 
  [Gate 6 10 0
  ,Gate 0 17 0
  ,Gate 9 13 0
  ,Gate 3 12 0
  ,Gate 2 4 0
  ,Gate 14 15 0
  ,Gate 1 7 0
  ,Gate 11 16 0
  ,Gate 5 8 0
  ,Gate 0 9 1
  ,Gate 3 13 1
  ,Gate 2 12 1
  ,Gate 4 14 1
  ,Gate 1 15 1
  ,Gate 7 11 1
  ,Gate 5 16 1
  ,Gate 8 17 1
  ] 

a :: [Int]
a = sort . map (windowSize) . filter ((>0).windowSize) . map elems . nubSort $ map (runNetwork netw) inputs' 
b :: [Int]
b = sort . map (windowSize) . filter ((>0).windowSize) . map elems . nubSort $ map (runNetwork netw2) inputs' 
netw2 :: [Gate 'Standard]
netw2 = untangle $ permuteChannels permut netw
netw :: [Gate 'Standard]
netw = [Gate 1 2 0,Gate 0 4 0,Gate 0 1 1,Gate 3 4 1]
permut :: Channel -> Channel
permut x = case x of
  0 -> 1
  1 -> 2
  2 -> 4
  3 -> 0
  4 -> 3

gf16inpts :: IO ()
gf16inpts = mapM_ (print . map fromEnum . elems) $ greenfilterInputs 16

gf16 :: [Gate 'Standard]
gf16 = greenfilter 16

revgf16 :: [Gate 'Standard]
revgf16 = revUntangle gf16

-- gf16saving = map f $ greenfilterInputs 16
--   where
--     f x = let rev

revUntangle :: [Gate 'Standard] -> [Gate 'Standard]
revUntangle = untangle . permuteChannels rev

pr :: IO ()
pr = print a >> print b

permuteChannels :: forall t. KnownNetType t => (Channel -> Channel) -> [Gate t] -> [Gate 'Generalized]
permuteChannels permChan = map permGate
  where
    permGate :: Gate t -> Gate 'Generalized
    permGate (Gate i j k) = Gate (permChan i) (permChan j) k

reflect :: [Gate 'Standard] -> [Gate 'Standard]
reflect = map refl
  where
    refl (Gate i j k) = Gate (maxBound - j) (maxBound - i) k

untangle :: [Gate 'Generalized] -> [Gate 'Standard]
untangle [] = []
untangle (Gate i j k:gates) 
  | i < j     = Gate i j k : untangle gates
  | otherwise = Gate j i k : untangle (permuteChannels (swap i j) gates)

swap :: Channel -> Channel -> Channel -> Channel
swap i j x
  | x == i = j
  | x == j = i
  | otherwise = x

swapHalves :: Channel -> Channel
swapHalves x = if x < fromIntegral (n`div`2) then x + fromIntegral (n`div`2) else x - fromIntegral (n`div`2)

reverse_prefix_18_1 :: [Gate 'Standard]
reverse_prefix_18_1 = untangle $ permuteChannels rev prefix18_1

-- assumes n is a power of 2
greenfilterInputs :: Int -> [UArray Channel Bool]
greenfilterInputs n'
    | isPowerOfTwo n' = gf n'
    | otherwise = error $ "argument must be a power of 2 but is " ++ show n'
  where
    isPowerOfTwo :: Int -> Bool
    isPowerOfTwo x = x .&. (n'-1) == 0
    filterLayers :: Int -> Int
    filterLayers channelCnt = (floor :: Double -> Int) . log $ fromIntegral channelCnt
    gf :: Int -> [UArray Channel Bool]
    gf 1 = [listArray (0,0) [False],listArray (0,0) [True]]
    gf w = nubOrd . map (runNetwork layer) $ do
        v1 <- gfHalve
        v2 <- gfHalve
        pure $ appendVec v1 v2
      where
        gfHalve :: [UArray Channel Bool]
        gfHalve = gf (w `div` 2)
        layer :: [Gate 'Standard]
        layer = [ Gate i (i + fromIntegral (w`div`2)) (fromIntegral $ filterLayers w) | i <- [ 0 .. fromIntegral (w `div` 2)-1 ] ]
        appendVec :: UArray Channel Bool -> UArray Channel Bool -> UArray Channel Bool
        appendVec v1 v2 = listArray (0, fromIntegral (w-1)) (elems v1 ++ elems v2)

-- something is wrong with the layer fields in the result
greenfilter :: Int -> [Gate 'Standard]
greenfilter n'
    | isPowerOfTwo n' = gf n'
    | otherwise = error $ "argument must be a power of 2 but is " ++ show n'
  where
    isPowerOfTwo :: Int -> Bool
    isPowerOfTwo x = x .&. (n'-1) == 0
    filterLayers :: Int -> Int
    filterLayers channelCnt = (floor :: Double -> Int) . log $ fromIntegral channelCnt
    gf :: Int -> [Gate 'Standard]
    gf w 
        | w == 2 = layer
        | otherwise = gfHalve ++ map (\(Gate i j k) -> Gate (i+fromIntegral w2) (j+fromIntegral w2) k) gfHalve ++ layer
      where
        w2 = w `div` 2
        gfHalve :: [Gate 'Standard]
        gfHalve = gf w2
        layer :: [Gate 'Standard]
        layer = [ Gate i (i + fromIntegral w2) (fromIntegral $ filterLayers w) | i <- [ 0 .. fromIntegral w2-1 ] ]

prefixCostPerm :: KnownNetType t => (Channel -> Channel) -> AddedInputs -> (UArray Channel Bool -> Int) -> [Gate t] -> Int
prefixCostPerm perm inps vecCost = prefixOutputsPerm perm inps (sum . map (vecCost . arra)) 

prefixOutputsPerm :: KnownNetType t => (Channel -> Channel) -> AddedInputs -> ([Input] -> a) -> [Gate t] -> a
prefixOutputsPerm perm inps f pref = f . filterInputs inps . nubSort . map runPrefix $ inputs'
  where
    runPrefix :: UArray Channel Bool -> Input
    runPrefix i = minimumBy (comparing (windowSize . elems . arra) <> comparing ty)
      [ MkInput Id $ runNetwork pref i
      , MkInput Reverse $ runNetwork reverse_pref . ixmap (minBound,maxBound) perm $ i
      ]
    reverse_pref :: [Gate 'Standard]
    reverse_pref = untangle $ permuteChannels perm pref
-- prefixCostPerm :: KnownNetType t => (Channel -> Channel) -> AddedInputs -> (UArray Channel Bool -> Int) -> [Gate t] -> Int
-- prefixCostPerm perm inps vecCost pref = sum . map (vecCost . arra) . filterInputs inps . nubSort . map runPrefix $ inputs'
--   where
--     runPrefix :: UArray Channel Bool -> Input
--     runPrefix i = minimumBy (comparing (windowSize . elems . arra) <> comparing arra)
--       [ MkInput Id $ runNetwork pref i
--       , MkInput Reverse $ runNetwork reverse_pref . ixmap (minBound,maxBound) perm $ i
--       ]
--     reverse_pref :: [Gate 'Standard]
--     reverse_pref = untangle $ permuteChannels perm pref

windowSizeDistributionPerm :: forall t. KnownNetType t => (Channel -> Channel) -> AddedInputs -> [Gate t] -> [(Int,Int)]
windowSizeDistributionPerm perm inps = prefixOutputsPerm perm inps winSizeDistr 
  

rev :: Channel -> Channel
rev = (maxBound -)
    -- rev = swap 0 1

shiftPerm :: Channel -> Channel
shiftPerm c = fromIntegral ((ci + (n`div`2)) `mod` n)
  where
    ci :: Int
    ci = fromIntegral c

perm :: [Channel] -> Channel -> Channel
perm p x = genericIndex p x

windowSizeDistributionRev :: KnownNetType t => AddedInputs -> [Gate t] -> [(Int, Int)]
windowSizeDistributionRev = windowSizeDistributionPerm rev

prefixCostRev :: KnownNetType t => AddedInputs -> (UArray Channel Bool -> Int) -> [Gate t] -> Int
prefixCostRev = prefixCostPerm rev

data PrefixNet = Id | Reverse
  deriving stock (Eq, Ord, Show)
data Input = MkInput { ty :: PrefixNet, arra :: UArray Channel Bool }
  deriving stock (Eq,Show)

instance Ord Input where
  compare = comparing (windowSize . elems . arra) <> comparing ty <> comparing (elems . arra)

-- prefixCostReverse :: KnownNetType t => (UArray Channel Bool -> Int) -> [Gate t] -> Int
-- prefixCostReverse vecCost pref = buildG . map f inputs'
--   -- sum . map vecCost . take 1619 . nubSort . map runPrefix $ inputs'
--   where
--     f :: UArray Channel Bool -> (Input, Input)
--     f inp = runNetwork pref
--       ( MkInput Id $ runNetwork pref $ inp
--       , MkInput Reverse $ runNetwork reverse_pref . ixmap (minBound,maxBound) (maxBound -) $ inp
--       )
--     runPrefix :: UArray Channel Bool -> UArray Channel Bool
--     runPrefix i = minimumBy (comparing (windowSize . elems) <> comparing elems)
--       [ runNetwork pref $ i
--       , runNetwork reverse_pref . ixmap (minBound,maxBound) (maxBound -) $ i
--       ]
--     reverse_pref = untangle $ permuteChannels (\i -> maxBound - i) pref

data AddedInputs = All | Best Int

filterInputs :: AddedInputs -> [a] -> [a]
filterInputs inps = case inps of
  All    -> id
  Best x -> take x

-- prefixCost :: KnownNetType t => AddedInputs -> (UArray Channel Bool -> Int) -> [Gate t] -> Int
-- prefixCost inps vecCost pref = sum . map vecCost . filterInputs inps . nubSort . map runPrefix $ inputs'
--   where
--     runPrefix :: UArray Channel Bool -> UArray Channel Bool
--     runPrefix = runNetwork pref


prefixOutputs :: KnownNetType t => AddedInputs -> ([Input] -> a) -> [Gate t] -> a
prefixOutputs inps f pref =  f . filterInputs inps . nubSort . map runPrefix $ inputs'
  where
    runPrefix :: UArray Channel Bool -> Input 
    runPrefix = MkInput Id . runNetwork pref

prefixCost :: KnownNetType t => AddedInputs -> (UArray Channel Bool -> Int) -> [Gate t] -> Int
prefixCost inps vecCost = prefixOutputs inps (sum . map (vecCost . arra))
prefixVarCost' :: [(Int, Int)] -> Int
prefixVarCost' = sum . map (uncurry (*)) 
prefixClauseCost' :: [(Int, Int)] -> Int
prefixClauseCost' = sum . map (\(windSize,cnt) -> windSize * windSize * cnt) 

winSizeDistr :: [Input] -> [(Int, Int)]
winSizeDistr = frequency . map (windowSize . elems . arra)

windowSizeDistribution :: KnownNetType t => AddedInputs -> [Gate t] -> [(Int, Int)]
windowSizeDistribution inps = prefixOutputs inps winSizeDistr 

-- number of clauses necessary to encode a SINGLE layer
clauseCnt :: UArray Channel Bool -> Int
clauseCnt = (\winSize -> winSize * winSize) . windowSize . elems

-- number of variables necessary to encode the state of the input in a SINGLE stage between the layers
variableCnt :: UArray Channel Bool -> Int
variableCnt = windowSize . elems

-- number of inputs
inputCnt :: UArray Channel Bool -> Int
inputCnt = const 1

green20Inputs :: [UArray Channel Bool]
green20Inputs = do
  v <- greenfilterInputs 16
  let (l,r) = splitAt 8 $ elems v
  v' <- zipWith (++) (inits $ replicate 4 False) (tails $ replicate 4 True)
  pure $ listArray (0,19) (l ++ v' ++ r)

-- green20ReverseInputs :: [UArray Channel Bool]
-- green20ReverseInputs = do
--   v <- un 16
--   let (l,r) = splitAt 8 $ elems v
--   v' <- zipWith (++) (inits $ replicate 4 False) (tails $ replicate 4 True)
--   pure $ listArray (0,19) (l ++ v' ++ r)
  
data InType = MinIn | MaxIn
  deriving stock (Eq, Ord, Show)

identifyGreenfilterOutput :: Channel -> [Gate 'Standard] -> Set [InType]
identifyGreenfilterOutput i net = Set.fromList . Map.elems $ foldr f (Map.singleton i []) net
  -- where
f :: Gate 'Standard -> Map Channel [InType] -> Map Channel [InType] 
f (Gate j l _) paths = case Map.lookup j paths of
  Just path -> Map.insert j (MinIn : path) . Map.insert l (MinIn : path) $ paths
  Nothing -> case Map.lookup l paths of
    Just path -> Map.insert j (MaxIn : path) . Map.insert l (MaxIn : path) $ paths
    Nothing -> paths 

identifyGreenfilterPermutation :: [Gate 'Standard] -> [Gate 'Standard] -> Channel -> Channel
identifyGreenfilterPermutation gf1 gf2 i = fromJust $ lookup identifiedStructure structures
  where
    structures :: [(Set [InType], Channel)]
    structures = map (\j -> (identifyGreenfilterOutput j gf1, j)) [ minBound .. maxBound ]
    identifiedStructure = identifyGreenfilterOutput i gf2

optGreenfilter32Perm :: Channel -> Channel
optGreenfilter32Perm = identifyGreenfilterPermutation (greenfilter 32) greenfilter32_opt

greenfilterOpt32Inputs :: [(Int, Int)]
greenfilterOpt32Inputs = winSizeDistr . map (MkInput Id . ixmap (minBound,maxBound) optGreenfilter32Perm) $ greenfilterInputs 32

greenfilterInputsWinSizeDistr :: [(Int, Int)]
greenfilterInputsWinSizeDistr = winSizeDistr . map (MkInput Id) $ greenfilterInputs 32

greenfilterOpt32InputsRev :: [(Int, Int)]
greenfilterOpt32InputsRev = winSizeDistr . map selectBetter $ greenfilterInputs 32
  where
    selectBetter x = minimumBy (comparing (windowSize . elems . arra) <> comparing ty)
      [ MkInput Id . ixmap (minBound,maxBound) optGreenfilter32Perm $ x
      -- , MkInput Reverse . ixmap (minBound,maxBound) (rev . optGreenfilter32Perm) $ x
      , MkInput Reverse x
      ]



prefix21 :: [Gate 'Standard]
prefix21 =
  [ Gate 5 13 0
  , Gate 0 9 0
  , Gate 10 16 0
  , Gate 1 7 0
  , Gate 15 19 0
  , Gate 3 11 0
  , Gate 2 14 0
  , Gate 6 8 0
  , Gate 17 18 0
  , Gate 4 12 0
  , Gate 0 10 1
  , Gate 1 16 1
  , Gate 7 15 1
  , Gate 3 19 1
  , Gate 2 11 1
  , Gate 6 14 1
  , Gate 8 17 1
  , Gate 4 18 1
  , Gate 9 20 1
  ]

-- easy prefix
prefix17_0 :: [Gate 'Standard]
prefix17_0 =
  [ Gate 3 13 0
  , Gate 11 12 0
  , Gate 6 16 0
  , Gate 1 5 0
  , Gate 9 10 0
  , Gate 2 15 0
  , Gate 4 14 0
  , Gate 0 7 0
  , Gate 3 11 1
  , Gate 12 13 1
  , Gate 1 6 1
  , Gate 5 16 1
  , Gate 2 9 1
  , Gate 10 15 1
  , Gate 0 4 1
  , Gate 7 14 1
  ]

-- hard prefix
prefix17_51 :: [Gate 'Standard]
prefix17_51 =
  [ Gate 4 16 0
  , Gate 0 10 0
  , Gate 7 11 0
  , Gate 8 13 0
  , Gate 1 12 0
  , Gate 2 15 0
  , Gate 3 14 0
  , Gate 5 9 0
  , Gate 0 4 1
  , Gate 7 10 1
  , Gate 8 11 1
  , Gate 1 13 1
  , Gate 2 12 1
  , Gate 3 15 1
  , Gate 5 14 1
  , Gate 9 16 1
  ]

greenfilter32_opt :: [Gate 'Standard] 
greenfilter32_opt = 
  [ Gate 5 20 0
  , Gate 0 11 0
  , Gate 1 4 0
  , Gate 7 13 0
  , Gate 8 15 0
  , Gate 18 28 0
  , Gate 12 21 0
  , Gate 2 24 0
  , Gate 9 29 0
  , Gate 16 22 0
  , Gate 6 23 0
  , Gate 3 14 0
  , Gate 19 25 0
  , Gate 17 26 0
  , Gate 10 30 0
  , Gate 27 31 0
  , Gate 0 5 1
  , Gate 11 20 1
  , Gate 1 7 1
  , Gate 4 13 1
  , Gate 8 18 1
  , Gate 15 28 1
  , Gate 2 12 1
  , Gate 21 24 1
  , Gate 9 16 1
  , Gate 22 29 1
  , Gate 3 6 1
  , Gate 14 23 1
  , Gate 17 19 1
  , Gate 25 26 1
  , Gate 10 27 1
  , Gate 30 31 1
  , Gate 0 1 2
  , Gate 4 11 2
  , Gate 5 7 2
  , Gate 13 20 2
  , Gate 2 8 2
  , Gate 15 21 2
  , Gate 12 18 2
  , Gate 24 28 2
  , Gate 3 9 2
  , Gate 14 22 2
  , Gate 6 16 2
  , Gate 23 29 2
  , Gate 10 17 2
  , Gate 25 30 2
  , Gate 19 27 2
  , Gate 26 31 2
  , Gate 0 2 3
  , Gate 4 15 3
  , Gate 5 12 3
  , Gate 13 24 3
  , Gate 1 8 3
  , Gate 11 21 3
  , Gate 7 18 3
  , Gate 20 28 3
  , Gate 3 10 3
  , Gate 14 25 3
  , Gate 6 19 3
  , Gate 23 26 3
  , Gate 9 17 3
  , Gate 22 30 3
  , Gate 16 27 3
  , Gate 29 31 3
  , Gate 0 3 4
  , Gate 4 14 4
  , Gate 5 6 4
  , Gate 13 23 4
  , Gate 1 9 4
  , Gate 11 22 4
  , Gate 7 16 4
  , Gate 20 29 4
  , Gate 2 10 4
  , Gate 15 25 4
  , Gate 12 19 4
  , Gate 24 26 4
  , Gate 8 17 4
  , Gate 21 30 4
  , Gate 18 27 4
  , Gate 28 31 4
  ]