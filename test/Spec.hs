{-# language DataKinds #-}
{-# language TypeApplications #-}
{-# language ScopedTypeVariables #-}
{-# language PolyKinds #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

import Test.Tasty
import Test.SmallCheck.Series
import Test.Tasty.SmallCheck

import Test.PreservesOrd
import Test.Ix
import Test.BoundedEnum
import Test.IxBoundedEnum

import GHC.TypeNats
import Data.Finite (Finite)
import Data.Typeable
import Data.Ix

import MinDepthSN.Data.Size () -- Ix instance for Finite
import Data.Pair.OrderAndDup
import Data.Pair
import Data.Pair.AbsDiffGT1

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ testPairs
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
testPair _o _d = testGroup "Pair tests"
    [ testSmallType (pairs @o @d @(Finite 5)) PreservesOrd
    , testProperty "pair ix range" $ ixPairs @o @d @(Finite 5)
    ]
    
testAbsDiffGT1 :: forall (o :: Order). Typeable o => Proxy o -> TestTree
testAbsDiffGT1 _o = testSmallType (absdiffgt1s @o @(Finite 5)) ViolatesOrd

testSmallType
  :: forall a. (Show a, Ix a, Bounded a, Enum a, Typeable a, Serial IO a)
  => [a] -> PreservesOrder -> TestTree
testSmallType vals ord = testGroup (showsTypeRep (typeRep (Proxy @a)) "") $
    [ testIxLaws @a Proxy
    , testBoundedEnum vals ord
    , testIxBoundedEnum @a Proxy
    ] ++ case ord of
        PreservesOrd -> [ testIxOrdLaws @a Proxy ]
        ViolatesOrd  -> []

ixPairs :: (Typeable o, Typeable d, Ix a) => Pair o d a -> Pair o d a -> Bool
ixPairs l u = range (l,u) == pairIxRange (l,u)
  where
    pairIxRange
        :: (Typeable o, Typeable d, Ix a) 
        => (Pair o d a, Pair o d a) -> [Pair o d a]
    pairIxRange (Pair l1 l2, Pair u1 u2) = pairRange (min l1 l2, max u1 u2)

pairs :: forall o d a. (Bounded a, Ix a, Typeable o, Typeable d) => [Pair o d a]
pairs = pairRange (minBound, maxBound)

pairRange :: forall o d a. (Ix a, Typeable o, Typeable d) => (a, a) -> [Pair o d a]
pairRange b = 
    [ Pair x y
    | x <- range b
    , y <- range b
    , x `cmp` y
    ]
  where
    cmp :: a -> a -> Bool
    cmp = case (orderAndDup :: OrderAndDup o d) of
        UND -> (<)
        OND -> (/=)
        UWD -> (<=)
        OWD -> const (const True)

absdiffgt1s :: forall o a. (Bounded a, Enum a, Ix a, Typeable o) => [AbsDiffGT1 o a] 
absdiffgt1s = absdiffgt1sRange (minBound, maxBound)

absdiffgt1sRange :: forall o a. (Enum a, Ix a, Typeable o) => (a, a) -> [AbsDiffGT1 o a] 
absdiffgt1sRange b =
    [ AbsDiffGT1 x y
    | x <- range b
    , y <- range b
    , x `cmp` y && abs (fromEnum x - fromEnum y) > 1
    ]
  where
    cmp :: a -> a -> Bool
    cmp = case (orderAndDup :: OrderAndDup o 'NoDuplicates) of
        UND -> (<=)
        OND -> (/=)

instance (Bounded a, Ix a, Typeable o, Typeable d, Monad m) => Serial m (Pair o d a) where
    series = generate $ \d -> take (d+1) pairs

instance (Bounded a, Enum a, Ix a, Typeable o, Monad m) => Serial m (AbsDiffGT1 o a) where
    series = generate $ \d -> take (d+1) absdiffgt1s

instance (KnownNat n, Monad m) => Serial m (Finite n) where
    series = generate $ \d -> take (d+1) [ minBound .. maxBound ]














