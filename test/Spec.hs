{-# language DataKinds #-}
{-# language TypeApplications #-}
{-# language ScopedTypeVariables #-}
{-# language PolyKinds #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Test.Tasty
import Test.Tasty.HUnit


-- import Test.Tasty.QuickCheck hiding (Ordered)
import Test.Tasty.SmallCheck
import Test.SmallCheck.Series

-- import Test.QuickCheck.Classes.Base
-- lawsCheck

import Data.Typeable
import Data.Ix

import Data.Pair.OrderedNoDuplicates
import Data.Pair.UnorderedNoDuplicates
import Data.Pair
import Data.Pair.OrderAndDup
import Data.Pair.AbsDiffGT1
import Data.Finite (Finite)
import GHC.TypeNats
import MinDepthSN.Data.Size () -- Ix instance for Finite


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
    , testPairs
    ]

testPairs :: TestTree
testPairs = testGroup "Pair"
    [ testPair @'Unordered @'NoDuplicates   Proxy Proxy
    , testPair @'Unordered @'WithDuplicates Proxy Proxy
    , testPair @'Ordered   @'NoDuplicates   Proxy Proxy
    , testPair @'Ordered   @'WithDuplicates Proxy Proxy
    , testAbsDiffGT1 @'Unordered Proxy
    , testAbsDiffGT1 @'Ordered   Proxy
    ]

testPair :: forall (o :: Order) (d :: Duplicates). (Typeable o, Typeable d) => Proxy o -> Proxy d -> TestTree
testPair _o _d = testLaws @(Pair o d (Finite 5)) Proxy
    
testAbsDiffGT1 :: forall (o :: Order). Typeable o => Proxy o -> TestTree
testAbsDiffGT1 _o = testLaws @(AbsDiffGT1 o (Finite 5)) Proxy

testLaws :: forall a. (Show a, Ix a, Bounded a, Enum a, Typeable a, Serial IO a) => Proxy a -> TestTree
testLaws _a = testGroup (showsTypeRep (typeRep (Proxy @a)) $ "")
    [ testIxLaws @a Proxy
    , testIxEnumBoundedLaws @a Proxy
    ]

testIxLaws :: forall a. (Show a, Ix a, Serial IO a) => Proxy a -> TestTree
testIxLaws _a = testGroup "Ix laws"
    [ testProperty "inRange" $ ixInRange @a Proxy
    , testProperty "rangeIndex" $ ixRangeIndex @a Proxy
    , testProperty "mapIndexRange" $ ixMapIndexRange @a Proxy
    , testProperty "rangeSize" $ ixRangeSize @a Proxy
    , testProperty "Ord" $ ixOrd @a Proxy
    , testProperty "bounds" $ ixBounds @a Proxy
    ]

testIxEnumBoundedLaws :: forall a. (Show a, Ix a, Enum a, Bounded a) => Proxy a -> TestTree
testIxEnumBoundedLaws _a = testGroup "IxEnumBounded laws"
    [ testCase "enumRange" $ range @a (minBound, maxBound) @?= [ minBound .. maxBound ]
    , testCase "size" $ rangeSize @a (minBound, maxBound) @?= fromEnum @a maxBound + 1
    ]


instance (Bounded a, Enum a, Ord a, Typeable o, Typeable d, Monad m) => Serial m (Pair o d a) where
    series = generate $ \d -> take (d+1) 
        [ Pair x y
        | x <- [ minBound .. maxBound ]
        , y <- [ minBound .. maxBound ]
        , x `cmp` y
        ]
      where
        cmp :: a -> a -> Bool
        cmp = case (orderAndDup :: OrderAndDup o d) of
            UND -> (<)
            OND -> (/=)
            UWD -> (<=)
            OWD -> const (const True)

instance (Bounded a, Enum a, Ord a, Typeable o, Monad m) => Serial m (AbsDiffGT1 o a) where
    series = generate $ \d -> take (d+1)
        [ AbsDiffGT1 x y
        | x <- [ minBound .. maxBound ]
        , y <- [ minBound .. maxBound ]
        , x `cmp` y && abs (fromEnum x - fromEnum y) > 1
        ]
      where
        cmp :: a -> a -> Bool
        cmp = case (orderAndDup :: OrderAndDup o 'NoDuplicates) of
            UND -> (<=)
            OND -> (/=)

instance (KnownNat n, Monad m) => Serial m (Finite n) where
    series = generate $ \d -> take (d+1) [ minBound .. maxBound ]


ixInRange :: forall a m. (Show a, Ix a, Monad m, Serial m a) => Proxy a -> Property m
ixInRange _a =
    over series $ \(l :: a) ->
        over series $ \(u :: a) ->
            over series $ \(i :: a) ->
                inRange (l,u) i == elem i (range (l,u))

ixRangeIndex :: forall a m. (Show a, Ix a, Monad m, Serial m a) => Proxy a -> Property m
ixRangeIndex _a =
    over series $ \(l :: a) ->
        over series $ \(u :: a) ->
            over series $ \(i :: a) ->
                inRange (l,u) i ==> range (l,u) !! index (l,u) i == i

ixMapIndexRange :: forall a m. (Show a, Ix a, Monad m, Serial m a) => Proxy a -> Property m
ixMapIndexRange _a =
    over series $ \(l :: a) ->
        over series $ \(u :: a) ->
            map (index (l,u)) (range (l,u)) == [0 .. rangeSize (l,u) - 1]

ixRangeSize :: forall a m. (Show a, Ix a, Monad m, Serial m a) => Proxy a -> Property m
ixRangeSize _a =
    over series $ \(l :: a) ->
        over series $ \(u :: a) ->
            rangeSize (l,u) == length (range (l,u))

ixOrd :: forall a m. (Show a, Ix a, Monad m, Serial m a) => Proxy a -> Property m
ixOrd _a =
    over series $ \(l :: a) ->
        over series $ \(u :: a) ->
            over series $ \(i :: a) ->
                over series $ \(j :: a) ->
                    inRange (l,u) i && inRange (l,u) j ==> (index (l,u) i <= index (l,u) j) == (i <= j)

ixBounds :: forall a m. (Show a, Ix a, Monad m, Serial m a) => Proxy a -> Property m
ixBounds _a =
    over series $ \(l :: a) ->
        over series $ \(u :: a) ->
            over series $ \(i :: a) ->
                inRange (l,u) i ==> l <= i && i <= u

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














