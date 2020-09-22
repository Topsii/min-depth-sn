{-# LANGUAGE FlexibleContexts #-}
{-# language TypeApplications #-}
{-# language ScopedTypeVariables #-}

module Test.IxBoundedEnum where

import Data.Ix
import Data.Proxy
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck
import Test.SmallCheck.Series

testIxBoundedEnum :: forall a. (Show a, Ix a, Enum a, Bounded a, Serial IO a) => Proxy a -> TestTree
testIxBoundedEnum _a = testGroup "IxBoundedEnum"
    [ testCase "enumRange" $ enumRange @a Proxy
    , testCase "size" $ size @a Proxy
    , testProperty "indexFromEnum" $ indexFromEnum @a Proxy
    ]

enumRange :: forall a. (Show a, Ix a, Enum a, Bounded a) => Proxy a -> Assertion
enumRange _a = range @a (minBound, maxBound) @=? [ minBound .. maxBound ]

size :: forall a. (Show a, Ix a, Enum a, Bounded a) => Proxy a -> Assertion
size _a = rangeSize @a (minBound, maxBound) @=? fromEnum @a maxBound + 1

indexFromEnum :: forall a m. (Show a, Ix a, Enum a, Bounded a, Monad m, Serial m a) => Proxy a -> Property m
indexFromEnum _a =
    over series $ \(x :: a) ->
            index (minBound,maxBound) x == fromEnum x