
module MinDepthSN.Data.Window where -- another module MinDepthSN.Data.Input?

import Data.Bits
import MinDepthSN.Data.Size
import Data.List (sortOn)

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


windowBounds :: [Bool] -> (Channel, Channel)
windowBounds input
    | isSorted input = (1, 0) -- ill defined
    | otherwise = (l, h)
  where
    l = fromIntegral $ leadingZeroes input
    h = fromIntegral $ n - 1 - trailingOnes input
