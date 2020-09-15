{-# language DataKinds #-}
{-# language TypeApplications #-}

import Test.Tasty
import Test.Tasty.HUnit

import Data.Pair.OrderedNoDuplicates
import Data.Pair.UnorderedNoDuplicates
import Data.Finite
import GHC.TypeNats

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ testGroup "ordered pair without duplicates"
        [ testCase "fromEnum (8)" $
            map fromEnum (orderedNoDups @ 8) @?= [0..55] -- 55 = 8*7-1
        , testCase "toEnum (8)" $
            map toEnum [0..55] @?= orderedNoDups @ 8
        , testCase "fromEnum (5)" $ 
            map fromEnum (orderedNoDups @ 5) @?= [0..19] -- 19 = 5*4-1
        , testCase "toEnum (5)" $
            map toEnum [0..19] @?= orderedNoDups @ 5
        ]
    , testGroup "unordered pair without duplicates"
        [ testCase "fromEnum (8)" $
            map fromEnum (unorderedNoDups @ 8) @?= [0..27] -- 27 = 8*7/2-1
        , testCase "toEnum (8)" $
            map toEnum [0..27] @?= unorderedNoDups @ 8
        , testCase "fromEnum (5)" $ 
            map fromEnum (unorderedNoDups @ 5) @?= [0..9] -- 9 = 5*4/2-1
        , testCase "toEnum (5)" $
            map toEnum [0..9] @?= unorderedNoDups @ 5
        ]
    ]

unorderedNoDups :: KnownNat n => [UnorderedNoDuplicates (Finite n)]
unorderedNoDups =
    [ UnorderedNoDuplicates x y
    | x <- [minBound .. maxBound]
    , y <- [minBound .. maxBound]
    , x < y
    ]

orderedNoDups :: KnownNat n => [OrderedNoDuplicates (Finite n)]
orderedNoDups =
    [ OrderedNoDuplicates x y
    | x <- [minBound .. maxBound]
    , y <- [minBound .. maxBound]
    , x /= y
    ]














