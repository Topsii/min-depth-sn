{-# language DataKinds #-}
{-# language TypeApplications #-}

import Test.Tasty
import Test.Tasty.HUnit

import MinDepthSN.Data.Combinatorics2.CombinationNoRepetition
import MinDepthSN.Data.Combinatorics2.VariationNoRepetition
import Data.Finite
import GHC.TypeNats

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ testGroup "VariationNoRepetition"
        [ testCase "fromEnum (8)" $
            map fromEnum (variationsNoRep @ 8) @?= [0..55] -- 55 = 8*7-1
        , testCase "toEnum (8)" $
            map toEnum [0..55] @?= variationsNoRep @ 8
        , testCase "fromEnum (5)" $ 
            map fromEnum (variationsNoRep @ 5) @?= [0..19] -- 19 = 5*4-1
        , testCase "toEnum (5)" $
            map toEnum [0..19] @?= variationsNoRep @ 5
        ]
    , testGroup "CombinationNoRepetition"
        [ testCase "fromEnum (8)" $
            map fromEnum (combinationsNoRep @ 8) @?= [0..27] -- 27 = 8*7/2-1
        , testCase "toEnum (8)" $
            map toEnum [0..27] @?= combinationsNoRep @ 8
        , testCase "fromEnum (5)" $ 
            map fromEnum (combinationsNoRep @ 5) @?= [0..9] -- 9 = 5*4/2-1
        , testCase "toEnum (5)" $
            map toEnum [0..9] @?= combinationsNoRep @ 5
        ]
    ]

combinationsNoRep :: KnownNat n => [CombinationNoRepetition (Finite n)]
combinationsNoRep =
    [ CombinationNoRepetition x y
    | x <- [minBound .. maxBound]
    , y <- [minBound .. maxBound]
    , x < y
    ]

variationsNoRep :: KnownNat n => [VariationNoRepetition (Finite n)]
variationsNoRep =
    [ VariationNoRepetition x y
    | x <- [minBound .. maxBound]
    , y <- [minBound .. maxBound]
    , x /= y
    ]

-- combinationsNoRepInRange :: Ord a => [CombinationNoRepetition a]





















