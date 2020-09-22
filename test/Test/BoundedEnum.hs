{-# language TypeApplications #-}
{-# language ScopedTypeVariables #-}

module Test.BoundedEnum where

import Test.PreservesOrd
import Test.Tasty
import Test.Tasty.HUnit

testBoundedEnum :: forall a. (Show a, Eq a, Ord a, Enum a, Bounded a) => [a] -> PreservesOrder -> TestTree
testBoundedEnum vals ord = testGroup "BoundedEnum"
    [ testCase "enumAll"  $ enumAll vals ord
    , testCase "fromEnum" $ enumFromEnum vals
    , testCase "toEnum"   $ enumToEnum vals ord
    ]

enumAll :: (Show a, Enum a, Bounded a, Ord a) => [a] -> PreservesOrder -> Assertion
enumAll vals ord = vals  @=? sortIfOrdViolated ord [ minBound .. maxBound ]

enumFromEnum :: forall a. (Show a, Enum a, Bounded a) => [a] -> Assertion
enumFromEnum vals = [ 0 .. length vals - 1 ] @=? map (fromEnum @a) [ minBound .. maxBound ]

enumToEnum :: (Show a, Enum a, Bounded a, Ord a) => [a] -> PreservesOrder -> Assertion
enumToEnum vals ord = vals @=? sortIfOrdViolated ord (map toEnum [ 0 .. length vals - 1 ])