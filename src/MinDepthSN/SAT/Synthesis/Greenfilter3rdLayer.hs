{-# LANGUAGE DataKinds #-}

module MinDepthSN.SAT.Synthesis.Greenfilter3rdLayer where

import SAT.IPASIR

import MinDepthSN.Vars
import MinDepthSN.PrefixCost

import Data.Bits (shiftL)
import Control.Monad (guard)


-- | creates the same prefix as in the pairwise sorting network. n stands for the 
-- number of channels. Note that the prefix is complete iff n is a power of 2.
-- To get more information about the prefix see:
-- Parberry, Ian (1992), "The Pairwise Sorting Network" (PDF), Parallel Processing Letters, 2 (2,3): 205–211
prefix :: (KnownNetType t) => [[Lit (NetworkSynthesis t)]]
prefix = do
    k <- takeWhile ((<= n) . shiftL 1) [1 ..]
    let shifter = shiftL 1 $ k - 1
    s <- takeWhile (< n-2) $ map (* (2 * shifter)) [0 ..]
    i <- (+ s) <$> [0 .. shifter-1]
    let j = i + shifter
    guard (j <= n-3)
    pure $ pure (gateLit (toEnum $i+1) (toEnum $ j+1) (toEnum $ k-1))

greenfilterPrefix :: KnownNetType t => [[Lit (NetworkSynthesis t)]]
greenfilterPrefix = map (pure . (\(Gate i j k) -> gateLit i j k)) $ greenfilter n

greenfilterPrefix12 :: KnownNetType t => [[Lit (NetworkSynthesis t)]]
greenfilterPrefix12 = map (pure . (\(Gate i j k) -> gateLit i j k)) . (++ [Gate 4 5 0, Gate 6 7 0, Gate 4 6 1, Gate 5 7 1, Gate 4 5 2, Gate 6 7 2]) . map skipMid4G $ greenfilter 8
  where
    skipMid4G :: Gate 'Standard -> Gate 'Standard
    skipMid4G (Gate i j k) = Gate (skipMid4 i) (skipMid4 j) k
    skipMid4 :: Channel -> Channel
    skipMid4 x =if x < 4 then x else x + 4

greenfilterPrefixMod :: KnownNetType t => [[Lit (NetworkSynthesis t)]]
greenfilterPrefixMod = map (pure . (\(Gate i j k) -> gateLit i j k)) . (++ [Gate 0 7 2, Gate 1 6 2, Gate 2 4 2, Gate 3 5 2]) . filter (\(Gate _ _ k) -> k <= 1) $ greenfilter n

-- greenfilterPrefixMod2 :: KnownNetType t => [[Lit (NetworkSynthesis t)]]
-- greenfilterPrefixMod2 = map (pure . (\(Gate i j k) -> gateLit i j k)) . untangle . permuteChannels interleaveChan . (++ [Gate 0 7 2, Gate 1 6 2, Gate 2 4 2, Gate 3 5 2]) . filter (\(Gate _ _ k) -> k <= 1) $ greenfilter n

greenfilterPrefixMod3 :: KnownNetType t => [[Lit (NetworkSynthesis t)]]
greenfilterPrefixMod3 = map (pure . (\(Gate i j k) -> gateLit i j k)) . (++ [Gate 0 7 2, Gate 1 6 2, Gate 2 4 2, Gate 3 5 2, Gate 8 15 2, Gate 9 14 2, Gate 10 12 2, Gate 11 13 2]) . filter (\(Gate _ _ k) -> k <= 1) $ greenfilter n


xx :: [GateOrUnused 'Standard]
xx = [Gate 0 1 0,Gate 2 3 0,Gate 4 5 0,Gate 6 7 0,Gate 8 9 0,Gate 10 11 0,Gate 12 13 0,Gate 14 15 0,Gate 0 2 1,Gate 1 3 1,Gate 4 6 1,Gate 5 7 1,Gate 8 10 1,Gate 9 11 1,Gate 12 14 1,Gate 13 15 1,Gate 0 4 2,Gate 1 5 2,Gate 2 6 2,Gate 3 7 2,Gate 8 12 2,Gate 9 13 2,Gate 10 14 2,Gate 11 15 2,Gate 0 8 3,Gate 1 9 3,Gate 2 10 3,Gate 3 11 3,Gate 4 12 3,Gate 5 13 3,Gate 6 14 3,Gate 7 15 3,Unused 0 4,Unused 0 5,Unused 0 6,Unused 0 7,Unused 0 8,Unused 15 4,Unused 15 5,Unused 15 6,Unused 15 7,Unused 15 8,Gate 1 2 4,Gate 3 12 4,Gate 4 8 4,Gate 5 10 4,Gate 6 9 4,Gate 7 11 4,Gate 13 14 4,Gate 1 4 5,Gate 2 8 5,Gate 3 10 5,Gate 5 9 5,Gate 6 12 5,Gate 7 13 5,Gate 11 14 5,Unused 1 6,Unused 1 7,Unused 1 8,Unused 14 6,Unused 14 7,Unused 14 8,Gate 2 4 6,Gate 3 5 6,Gate 6 8 6,Gate 7 9 6,Gate 10 12 6,Gate 11 13 6,Unused 4 7,Unused 11 7,Gate 3 6 7,Gate 5 8 7,Gate 7 10 7,Gate 9 12 7,Gate 3 4 8,Gate 5 6 8,Gate 7 8 8,Gate 9 10 8,Gate 11 12 8]