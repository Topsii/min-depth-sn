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
    [ testProperty "inRange" $ ixInRange @a Proxy
    , testProperty "rangeIndex" $ ixRangeIndex @a Proxy
    , testProperty "mapIndexRange" $ ixMapIndexRange @a Proxy
    , testProperty "rangeSize" $ ixRangeSize @a Proxy
    , testProperty "lowerBounds" $ ixLowerBounds @a Proxy
    , testProperty "upperBounds" $ ixUpperBounds @a Proxy
    ]

testIxOrdLaws :: forall a. (Show a, Ix a, Serial IO a) => Proxy a -> TestTree
testIxOrdLaws _a = testGroup "Ix respecting Ord laws"
    [ testProperty "sortRange" $ ixSortRange @a Proxy
    , testProperty "index respects Ord" $ ixIndexOrd @a Proxy
    ]

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

ixSortRange :: forall a m. (Show a, Ix a, Monad m, Serial m a) => Proxy a -> Property m
ixSortRange _a =
    over series $ \(l :: a) ->
        over series $ \(u :: a) ->
            range (l,u) == sort (range (l,u))

ixIndexOrd :: forall a m. (Show a, Ix a, Monad m, Serial m a) => Proxy a -> Property m
ixIndexOrd _a =
    over series $ \(l :: a) ->
        over series $ \(u :: a) ->
            over series $ \(i :: a) ->
                over series $ \(j :: a) ->
                    inRange (l,u) i && inRange (l,u) j ==> (index (l,u) i <= index (l,u) j) == (i <= j)

ixLowerBounds :: forall a m. (Show a, Ix a, Monad m, Serial m a) => Proxy a -> Property m
ixLowerBounds _a =
    over series $ \(l :: a) ->
    over series $ \(u :: a) ->
    over series $ \(m :: a) ->
        rangeSize (l,u) > 0 && rangeSize (l,m) > 0 ==> head (range (l,u)) == head (range (l,m))

ixUpperBounds :: forall a m. (Show a, Ix a, Monad m, Serial m a) => Proxy a -> Property m
ixUpperBounds _a =
    over series $ \(l :: a) ->
    over series $ \(u :: a) ->
    over series $ \(m :: a) ->
        rangeSize (l,u) > 0 && rangeSize (m,u) > 0 ==> last (range (l,u)) == last (range (m,u))

-- ixBounds :: forall a m. (Show a, Ix a, Monad m, Serial m a) => Proxy a -> Property m
-- ixBounds _a =
--     over series $ \(l :: a) ->
--         over series $ \(u :: a) ->
--             over series $ \(i :: a) ->
--                 inRange (l,u) i ==> l <= i && i <= u