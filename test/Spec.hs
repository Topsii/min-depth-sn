{-# language DataKinds #-}
{-# language TypeApplications #-}
{-# language ScopedTypeVariables #-}
{-# language PolyKinds #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Test.Tasty
import Test.SmallCheck.Series

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
testPair _o _d = testSmallType (pairs @o @d @(Finite 5)) PreservesOrd
    
testAbsDiffGT1 :: forall (o :: Order). Typeable o => Proxy o -> TestTree
testAbsDiffGT1 _o = testSmallType (absdiffgt1s @o @(Finite 5)) ViolatesOrd

testSmallType :: forall a. (Show a, Ix a, Bounded a, Enum a, Typeable a, Serial IO a) => [a] -> PreservesOrder -> TestTree
testSmallType vals ord = testGroup (showsTypeRep (typeRep (Proxy @a)) $ "") $
    [ testIxLaws @a Proxy
    , testBoundedEnum vals ord
    , testIxBoundedEnum @a Proxy
    ] ++ case ord of
        PreservesOrd -> [ testIxOrdLaws @a Proxy ]
        ViolatesOrd  -> []



pairs :: forall o d a. (Bounded a, Enum a, Ord a, Typeable o, Typeable d) => [Pair o d a]
pairs = 
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

absdiffgt1s :: forall o a. (Bounded a, Enum a, Ord a, Typeable o) => [AbsDiffGT1 o a] 
absdiffgt1s =
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

instance (Bounded a, Enum a, Ord a, Typeable o, Typeable d, Monad m) => Serial m (Pair o d a) where
    series = generate $ \d -> take (d+1) pairs

instance (Bounded a, Enum a, Ord a, Typeable o, Monad m) => Serial m (AbsDiffGT1 o a) where
    series = generate $ \d -> take (d+1) absdiffgt1s

instance (KnownNat n, Monad m) => Serial m (Finite n) where
    series = generate $ \d -> take (d+1) [ minBound .. maxBound ]














