{-# language MultiParamTypeClasses #-}
{-# language DerivingVia #-}
{-# language DeriveGeneric #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}
{-# language FlexibleContexts #-}



module MinDepthSN.Data.Gate
    ( Gate(Gate)
    , SortingOrder(..)
    , SortOrder
    , sortOrder
    , gateLit
    ) where

import Data.Proxy
import Data.Ix
import Type.Reflection
import GHC.Generics (Generic)
import Generic.Data
import SAT.IPASIR (AsVar(..), Lit, lit)
import MinDepthSN.Data.Size (Channel, Layer)
import MinDepthSN.Data.Combinatorics2.CombinationNoRepetition
import MinDepthSN.Data.Combinatorics2.VariationNoRepetition

data SortingOrder = Standard | Generalized
    deriving stock (Eq, Show)

type GateChannelSelection = CombinationNoRepetition

class SortOrder f where
    toGateChannels :: Proxy f -> Channel -> Channel -> f Channel
    fromGateChannels :: f Channel -> (Channel, Channel)

instance SortOrder CombinationNoRepetition where
    toGateChannels = const CombinationNoRepetition
    fromGateChannels (CombinationNoRepetition i j) = (i, j)

instance SortOrder VariationNoRepetition where
    toGateChannels = const VariationNoRepetition
    fromGateChannels (VariationNoRepetition i j) = (i, j)

sortOrder :: SortingOrder
sortOrder
    | someTypeRep (Proxy :: Proxy GateChannelSelection) == someTypeRep (Proxy :: Proxy CombinationNoRepetition) = Standard
    | someTypeRep (Proxy :: Proxy GateChannelSelection) == someTypeRep (Proxy :: Proxy VariationNoRepetition) =  Generalized
    | otherwise = error "the sort order must either be standard or generalized"

-- | @Gate i j k@ creates a variable \(g_{i,j}^k\) representing a
-- comparator gate where \(i\) and \(j\) are the channels and \(k\) is the layer.
data Gate = MkGate { layer :: Layer, channels :: GateChannelSelection Channel }
    deriving stock (Generic, Eq, Ord, Ix)
    deriving Enum via (FiniteEnumeration Gate)
    deriving Bounded via (Generically Gate)

{-# COMPLETE Gate #-}
pattern Gate :: Channel -> Channel -> Layer -> Gate
pattern Gate i j k <- MkGate k  (fromGateChannels -> (i,j))
  where
    Gate i j k = MkGate k (toGateChannels (Proxy :: Proxy GateChannelSelection) i j)

instance Show Gate where
        show (Gate i j k) = show sortOrder ++ "Gate " 
            ++ show i ++ " " ++ show j ++ " " ++ show k

-- | Literal of 'Gate' with positive polarity.
gateLit :: AsVar v Gate => Channel -> Channel -> Layer -> Lit v
gateLit i j k = lit (Gate i j k)
