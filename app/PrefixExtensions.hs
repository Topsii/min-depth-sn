{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wwarn #-}

module PrefixExtensions where


import MinDepthSN.Vars
import MinDepthSN.CEGIS ( networkSolution )
import MinDepthSN.PrefixCost

import Data.List (permutations)
import Data.Containers.ListUtils ( nubOrd )
import Data.Set (Set)
import qualified Data.Set as Set

nett :: [GateOrUnused 'Standard]
nett = ne where (Right ne) = networkSolution    
net :: [Gate 'Standard]
net = map (\(Gate i j k) -> Gate i j k) . filter (\(GateOrUnused i j _k) -> i /= j) $ nett

-- prefix 
-- filter



sw2third :: Channel -> Channel
sw2third x = case x of
    0 -> 0
    1 -> 1
    2 -> 2
    3 -> 3
    4 -> 8
    5 -> 9
    6 -> 10
    7 -> 11
    8 -> 4
    9 -> 5
    10 -> 6
    11 -> 7

newtype Network = MkNetwork [Set (Gate 'Standard)]
  deriving (Eq,Show, Ord)

toNetwork :: [Gate 'Standard] -> Network
toNetwork = MkNetwork . toLayers

toLayers :: [Gate 'Standard] -> [Set (Gate 'Standard)]
toLayers (Gate i j k:s@(Gate h m l:_))
    | k == l = let (l:ls) = toLayers s in Set.insert (Gate i j k) l : ls
    | k < l = let (ls) = toLayers s in Set.singleton (Gate i j k) : ls
toLayers [g] = [Set.singleton g]
toLayers [] = []

net2Lpref :: [Set (Gate 'Standard)]
net2Lpref = take 2 . toLayers $ net
net1Lpref :: [Set (Gate 'Standard)]
net1Lpref = take 1 . toLayers $ net

sol :: [Network]
sol = nubOrd . filter (\(MkNetwork ne) -> take 2 ne == net2Lpref) . map (toNetwork . (\p -> untangle $ permuteChannels p net) . perm) $ permutations [minBound .. maxBound]

sols :: [[Gate 'Standard]]
sols = map (foldMap Set.toList . (\(MkNetwork x) -> x)) sol

solCnt :: Int
solCnt = length . nubOrd .  map (toNetwork . (\p -> untangle $ permuteChannels p net) . perm) $ permutations [minBound .. maxBound]

-- solCntDistinc2Lpref = length . nubOrd .  map (toLayers . (\p -> untangle $ permuteChannels p net) . perm) $ permutations [minBound .. maxBound]
