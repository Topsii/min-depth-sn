{-# language TypeApplications #-}
{-# language ScopedTypeVariables #-}
{-# language FlexibleContexts #-}



module Test.Ix where

import Data.Ix
import Data.Proxy
import Data.List (sort)
import Test.Tasty
import Test.Tasty.SmallCheck
import Test.SmallCheck.Series

testIxLaws :: forall a. (Show a, Ix a, Serial IO a) => Proxy a -> TestTree
testIxLaws _a = testGroup "Ix laws"
    [ testProperty "inRange" $ ixInRange @a
    , testProperty "rangeIndex" $ ixRangeIndex @a
    , testProperty "mapIndexRange" $ ixMapIndexRange @a
    , testProperty "rangeSize" $ ixRangeSize @a
    , testProperty "lowerBounds" $ ixLowerBounds @a
    , testProperty "upperBounds" $ ixUpperBounds @a
    ]

testIxOrdLaws :: forall a. (Show a, Ix a, Serial IO a) => Proxy a -> TestTree
testIxOrdLaws _a = testGroup "Ix respecting Ord laws"
    [ testProperty "sortRange" $ ixSortRange @a
    , testProperty "index respects Ord" $ ixIndexOrd @a
    ]

ixInRange :: Ix a => a -> a -> a -> Bool
ixInRange l u i = inRange (l,u) i == elem i (range (l,u))

ixRangeIndex :: (Ix a, Monad m) => a -> a -> a -> Property m
ixRangeIndex l u i = inRange (l,u) i ==> range (l,u) !! index (l,u) i == i

ixMapIndexRange :: Ix a => a -> a -> Bool
ixMapIndexRange l u = map (index (l,u)) (range (l,u)) == [0 .. rangeSize (l,u) - 1]

ixRangeSize :: Ix a => a -> a -> Bool
ixRangeSize l u = rangeSize (l,u) == length (range (l,u))
            
ixSortRange :: Ix a => a -> a -> Bool
ixSortRange l u = range (l,u) == sort (range (l,u))

ixIndexOrd :: (Ix a, Monad m) => a -> a -> a -> a -> Property m
ixIndexOrd l u i j = 
    inRange (l,u) i && inRange (l,u) j ==> (index (l,u) i <= index (l,u) j) == (i <= j)

ixLowerBounds :: (Ix a, Monad m) => a -> a -> a -> Property m
ixLowerBounds l u m = 
    rangeSize (l,u) > 0 && rangeSize (l,m) > 0 ==> head (range (l,u)) == head (range (l,m))

ixUpperBounds :: (Ix a, Monad m) => a -> a -> a -> Property m
ixUpperBounds l u m = 
    rangeSize (l,u) > 0 && rangeSize (m,u) > 0 ==> last (range (l,u)) == last (range (m,u))

-- ixBounds :: forall a m.  (Ix a, Monad m) => a -> a -> a -> Property m
-- ixBounds l u i = inRange (l,u) i ==> l <= i && i <= u