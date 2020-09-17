{-# language MultiParamTypeClasses #-}
{-# language DerivingVia #-}
{-# language DeriveGeneric #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}
{-# language FlexibleContexts #-}
{-# language KindSignatures #-}
{-# language DataKinds #-}
{-# language TypeFamilies #-}
{-# language ScopedTypeVariables #-}

module MinDepthSN.Data.Gate
    ( Gate(Gate)
    , gateLit
    , KnownNetType
    , AreGateChannelsOrdered
    , NetworkType(..)
    ) where

import Data.Proxy
import Data.Ix
import Data.Typeable 
import Generic.Data
import SAT.IPASIR (AsVar(..), Lit, lit)
import MinDepthSN.Data.Size (Channel, Layer)
import Data.Pair

data NetworkType = Standard | Generalized
    deriving stock (Eq, Show)

type family AreGateChannelsOrdered (t :: NetworkType) :: Order where
    AreGateChannelsOrdered 'Standard    = 'Unordered
    AreGateChannelsOrdered 'Generalized = 'Ordered

class ( Typeable t
      , Typeable (AreGateChannelsOrdered t)
      ) => KnownNetType (t :: NetworkType) where

instance KnownNetType 'Standard
instance KnownNetType 'Generalized

-- | @Gate i j k@ creates a variable \(g_{i,j}^k\) representing a
-- comparator gate where \(i\) and \(j\) are the channels and \(k\) is the layer.
data Gate (t :: NetworkType)
    = MkGate Layer (Pair (AreGateChannelsOrdered t) 'NoDuplicates Channel)
    deriving stock (Generic, Eq, Ord, Ix)
    deriving Enum via FiniteEnumeration (Gate t)
    deriving Bounded via Generically (Gate t)

{-# COMPLETE Gate #-}
pattern Gate :: KnownNetType t => Channel -> Channel -> Layer -> Gate t
pattern Gate i j k = MkGate k (Pair i j)

instance KnownNetType t => Show (Gate t) where
    showsPrec p (Gate i j k) = showParen (p >= 11) $
        showsTypeRep (typeRep (Proxy :: Proxy t)) .
        showString "Gate " .
        showsPrec 11 i . showChar ' ' .
        showsPrec 11 j . showChar ' ' . 
        showsPrec 11 k

-- | Literal of 'Gate' with positive polarity.
gateLit :: forall v t. (KnownNetType t, AsVar (v t) (Gate t)) => Channel -> Channel -> Layer -> Lit (v t)
gateLit i j k = lit (Gate i j k :: Gate t)
