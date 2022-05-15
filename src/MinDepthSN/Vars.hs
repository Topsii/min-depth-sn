{-# language DeriveGeneric #-}
{-# language DerivingVia #-}

{-# LANGUAGE PatternSynonyms #-}

{-# language ViewPatterns #-}


{-# language ScopedTypeVariables #-}
{-# language KindSignatures #-}
{-# language DataKinds #-}
{-# language InstanceSigs #-}


{-# language GeneralizedNewtypeDeriving #-}



{-# language GADTs #-} --maybe use this for existientially quantified offsets of Values in NetworkSynthesis

{-# language FlexibleInstances #-} -- for AsValue instance in NetworkSynthesis

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-} -- pattern synonyms are intentionally bundled with multiple types

module MinDepthSN.Vars
    ( module MinDepthSN.Data.Size
    , module MinDepthSN.Data.NetworkType
    -- * Unused
    , Unused ( Unused )
    , UnusedAs()
    , unused_
    , unusedLit
    -- * Gate
    , Gate ( Gate )
    , GateAs()
    , gate_
    , revGate
    , gateLit
    -- * GateOrUnused
    , GateOrUnused ( GateOrUnused, Gate, Unused )
    , GateOrUnusedAs(..)
    , gateOrUnused_
    , gateOrUnusedLit
    -- * ToOneInUpTo / FromOneInUpTo
    , ToOneInUpTo
        ( ToOneInUpTo
        )
    , FromOneInUpTo ( FromOneInUpTo )
    , ToOneInUpToAs(..)
    , FromOneInUpToAs(..)
    , toOneInUpTo_
    , fromOneInUpTo_
    , toOneInUpToLit
    , fromOneInUpToLit
    , toOneInUpToLit'
    , fromOneInUpToLit'
    -- * ToBetweenBefore
    , ToBetweenBefore ( ToBetweenBefore )
    , ToBetweenBeforeAs(..)
    , toBetweenBefore_
    , toBetweenBeforeLit
    -- * ViaWrongTwist
    , ViaWrongTwist ( ViaWrongTwist )
    , ViaWrongTwistAs(..)
    , viaWrongTwist_
    , viaWrongTwistLit
    -- * SortedRelation
    , SortedRel ( SortedRel )
    , SortedRelAs(..)
    , sortedRel_
    , sortedRelLit
    -- * Value
    , Value ( Value )
    , ValueAs(..)
    , value_
    , inputValues
    , outputValues
    , valueLit
    -- * NetworkSynthesis
    , NetworkSynthesis
        ( Gate
        , Unused
        , GateOrUnused
        , ToBetweenBefore
        , ViaWrongTwist
        , SortedRel
        , Value
        )
    -- * CexRun
    , CexRun ( Value )
    ) where

import Data.Ix
import Generic.Data
-- import SAT.IPASIR (AsVar(..), Lit, lit)
import SAT.IPASIR (Var(..), Dimacs(..), Lit(PosLit))
import MinDepthSN.Data.Size

import Optics.Core ((%),  preview, prism, review, Prism' )


import Data.Pair
import qualified Data.Pair.AbsDiffGT1 as Pair
import Data.Proxy
import Data.Typeable ( showsTypeRep, typeRep )
import Data.Word (Word32)

import MinDepthSN.Data.NetworkType
import Control.Monad ((>=>))


-- #############################################################################
-- #############################################################################

-- | @Unused i k@ creates a variable \(unused_i^k\) indicating a
-- channel \(i\) is not used by any comparator gate in layer \(k\).
data Unused = MkUnused Layer Channel
    deriving stock (Eq, Generic, Ord, Ix)
    deriving Enum via (FiniteEnumeration Unused)
    deriving Bounded via (Generically Unused)

class UnusedAs u where
    unusedPrism :: Prism' u Unused

instance UnusedAs Unused where
    unusedPrism = prism id Right

{-# COMPLETE Unused :: Unused #-}
pattern Unused :: UnusedAs u => Channel -> Layer -> u
pattern Unused i k <- (preview unusedPrism -> Just (MkUnused k i))
  where Unused i k = unused_ $ MkUnused k i

unused_ :: UnusedAs u => Unused -> u
unused_ u = review unusedPrism u

instance Show Unused where
    showsPrec p (Unused i k) = showParen (p >= 11) $
        showString "Unused " . showsPrec 11 i . showChar ' ' . showsPrec 11 k 

-- | Literal of 'Unused' with positive polarity.
unusedLit :: UnusedAs u => Channel -> Layer -> Lit u
unusedLit i k = PosLit $ Unused i k


-- #############################################################################
-- #############################################################################

{- | @Gate i j k@ creates a variable \(g_{i,j}^k\) representing a
comparator gate where \(i\) is the min channel, \(j\) is the max channel and
 \(k\) is the layer.

    * For standard gates we have that \( i < j \).

    * For generalized gates we have that \( i \neq j \). In this
    case \(g_{i,j}^k\) is 

        - a min-max gate if \( i < j \)
        
        - a max-min gate if \( i > j \)
-}
data Gate (t :: NetworkType)
    = MkGate Layer (Pair (AreGateChannelsOrdered t) 'NoDuplicates Channel)
    deriving stock (Generic, Eq, Ord, Ix)
    deriving Enum via FiniteEnumeration (Gate t)
    deriving Bounded via Generically (Gate t)

class GateAs g where
    -- KnownNetType is only needed for GateAs GateOrUnused instance
    gatePrism :: forall (t :: NetworkType). KnownNetType t => Prism' (g t) (Gate t)

instance GateAs Gate where
    -- KnownNetType constraint is available according to class definition.
    gatePrism :: forall t. Prism' (Gate t) (Gate t)
    gatePrism = prism id Right

{-# COMPLETE Gate :: Gate #-}
pattern Gate :: (GateAs g, KnownNetType t) => Channel -> Channel -> Layer -> g t
pattern Gate i j k <- (preview gatePrism -> Just (MkGate k (Pair i j)))
  where Gate i j k = gate_ $ MkGate k (Pair i j)

gate_ :: (GateAs g, KnownNetType t) => Gate t -> g t
gate_ = review gatePrism 

instance KnownNetType t => Show (Gate t) where
    showsPrec p (Gate i j k) = showParen (p >= 11) $
        showsTypeRep (typeRep (Proxy :: Proxy t)) .
        showString "Gate " .
        showsPrec 11 i . showChar ' ' .
        showsPrec 11 j . showChar ' ' . 
        showsPrec 11 k

revGate :: (GateAs g, KnownNetType t) => Channel -> Channel -> Layer -> g t
revGate i j k = Gate (maxBound - i) (maxBound - j) k

-- | Literal of 'Gate' with positive polarity.
gateLit :: (KnownNetType t, GateAs g) => Channel -> Channel -> Layer -> Lit (g t)
gateLit i j k = PosLit $ Gate i j k


-- #############################################################################
-- #############################################################################

-- | A variable \(gu_{i,j}^k\) is either a comparator \(g\)ate variable or an 
-- \(u\)nused variable.
--
-- \[
-- gu_{i,j}^k :=    
--      \begin{cases}
--          g_{i,j}^k  & \text{if}\ i \neq j \\
--          unused_i^k & \text{otherwise}
--      \end{cases}
-- \]
--
-- For \(g_{i,j}^k\) see 'MinDepthSN.Data.Gate.Gate' and for \(unused_i^k\) see 'MinDepthSN.Data.Unused.Unused'
--
-- This enables a terser definition of the \(used\), \(once\) and \(update\) constraints
-- from the original encoding of Bundala and Zavodny.
-- See 'MinDepthSN.SAT.Synthesis.Constraints.usageOnce' and 'MinDepthSN.SAT.Synthesis.Constraints.unused'
--
-- The solver does not actually create, consider or solve \(gu_{i,j}^k\) 
-- variables. For any occurrence of \(gu_{i,j}^k\) in formulas or clauses, its 
-- definition is passed to the solver instead.
data GateOrUnused (t :: NetworkType) =
    MkGateOrUnused Layer (Pair (AreGateChannelsOrdered t) 'WithDuplicates Channel)
    deriving stock (Generic, Eq, Ord, Ix)
    deriving Enum via (FiniteEnumeration (GateOrUnused t))
    deriving Bounded via (Generically (GateOrUnused t))

class GateOrUnusedAs gu where
    gateOrUnusedPrism :: Prism' (gu t) (GateOrUnused t)

instance GateOrUnusedAs GateOrUnused where
    gateOrUnusedPrism = prism id Right

{-# COMPLETE GateOrUnused :: GateOrUnused #-}
pattern GateOrUnused :: (GateOrUnusedAs gu, KnownNetType t) => Channel -> Channel -> Layer -> gu t
pattern GateOrUnused i j k <- (preview gateOrUnusedPrism -> Just (MkGateOrUnused k (Pair i j)))
  where GateOrUnused i j k = gateOrUnused_ (MkGateOrUnused k (Pair i j))

gateOrUnused_ :: (GateOrUnusedAs gu, KnownNetType t) => GateOrUnused t -> gu t
gateOrUnused_ = review gateOrUnusedPrism

{-# COMPLETE Gate, Unused :: GateOrUnused #-}

instance GateAs GateOrUnused where
    gatePrism :: forall t. KnownNetType t => Prism' (GateOrUnused t) (Gate t)
    gatePrism = prism constructGate matchGate
      where
        constructGate :: Gate t -> GateOrUnused t
        constructGate (Gate i j k) = GateOrUnused i j k
        matchGate :: GateOrUnused t -> Either (GateOrUnused t) (Gate t)
        matchGate gu@(GateOrUnused i j k)
            | i /= j    = Right $ Gate i j k
            | otherwise = Left gu

instance KnownNetType t => UnusedAs (GateOrUnused t) where
    unusedPrism :: Prism' (GateOrUnused t) Unused
    unusedPrism = prism constructUnused matchUnused
      where
        constructUnused :: Unused -> GateOrUnused t
        constructUnused (Unused i k) = GateOrUnused i i k
        matchUnused :: GateOrUnused t -> Either (GateOrUnused t) Unused
        matchUnused gu@(GateOrUnused i j k)
            | i == j    = Right $ Unused i k
            | otherwise = Left gu

instance KnownNetType t => Show (GateOrUnused t) where
    showsPrec p (Gate i j k)   = showParen (p >= 11) $ showString "Gate_ "   . showsPrec 11 (Gate i j k :: Gate t) 
    showsPrec p (Unused i k) = showParen (p >= 11) $ showString "Unused_ " . showsPrec 11 (Unused i k :: Unused)

-- | Literal of 'GateOrUnused' with positive polarity.
gateOrUnusedLit :: (KnownNetType t, GateOrUnusedAs gu) => Channel -> Channel -> Layer -> Lit (gu t)
gateOrUnusedLit i j k = PosLit $ GateOrUnused i j k


-- #############################################################################
-- #############################################################################

{- | @ToOneInUpTo i j k@ creates a variable \(t_{i,j}^k\) with \( i \neq j \) 
representing the fact that there is a gate with min channel \(l\) and max
channel \(j\) in layer \(k\) where \(l\) is in the interval from \(j\) up
to \(i\) exclusive \(j\) and inclusive \(i\), i.e. 
  
  * \(t_{i,j}^k\) with \( i < j \) indicates there is some min-max gate \( g_{l,j}^k\) with \(i \le l < j \)
  in layer \(k\)

  * \(t_{i,j}^k\) with \( i > j \) indicates there is some max-min gate \( g_{l,j}^k\) with \(i \ge l > j \)
  in layer \(k\)

  See 'FromOneInUpTo' for the motivation of this variable.
-}
data ToOneInUpTo (t :: NetworkType) = MkToOneInUpTo Layer (Pair (AreGateChannelsOrdered t) 'NoDuplicates Channel)
    deriving stock (Generic, Eq, Ord, Ix)
    deriving Enum via (FiniteEnumeration (ToOneInUpTo t))
    deriving Bounded via (Generically (ToOneInUpTo t))

{- | @FromOneInUpTo i j k@ creates a variable \(f_{i,j}^k\) with \( i \neq j \) 
representing the fact that there is a gate with min channel \(i\) and max
channel \(l\) in layer \(k\) where \(l\) is in the interval from \(i\) up
to \(j\) exclusive \(i\) and inclusive \(j\), i.e. 
  
  * \(f_{i,j}^k\) with \( i < j \) indicates there is some min-max gate \( g_{i,l}^k\) with \(i < l \le j \)
  in layer \(k\)

  * \(f_{i,j}^k\) with \( i > j \) indicates there is some max-min gate \( g_{i,l}^k\) with \(i > l \ge j \)
  in layer \(k\)

  With respect to standard networks \(t_{i,j}^k\) and \(f_{i,j}^k\) correspond to the \( oneDown_{i,j}^k \)
  and \( oneUp_{i,j}^k \) variables from the paper New Bounds on Optimal Sorting Networks republished as
  Sorting Networks: to the End and Back Again. Unfortunately both publications 
  contain the same inconsistencies in their formalization of the variables.
  However they are (hopefully) able to convey the motivation for the
  variables.
-}
data FromOneInUpTo (t :: NetworkType) = MkFromOneInUpTo Layer (Pair (AreGateChannelsOrdered t) 'NoDuplicates Channel)
    deriving stock (Generic, Eq, Ord, Ix)
    deriving Enum via (FiniteEnumeration (FromOneInUpTo t))
    deriving Bounded via (Generically (FromOneInUpTo t))

class ToOneInUpToAs f where
    toOneInUpToPrism :: KnownNetType t => Prism' (f t) (ToOneInUpTo t)

class FromOneInUpToAs f where
    fromOneInUpToPrism :: KnownNetType t => Prism' (f t) (FromOneInUpTo t)

instance ToOneInUpToAs ToOneInUpTo where
    toOneInUpToPrism = prism id Right

instance FromOneInUpToAs FromOneInUpTo where
    fromOneInUpToPrism = prism id Right

{-# COMPLETE ToOneInUpTo :: ToOneInUpTo #-}
pattern ToOneInUpTo :: (ToOneInUpToAs f, KnownNetType t) => Channel -> Channel -> Layer -> (f t)
pattern ToOneInUpTo i j k <- (preview toOneInUpToPrism -> Just (MkToOneInUpTo k (Pair i j)))
  where ToOneInUpTo i j k = review toOneInUpToPrism (MkToOneInUpTo k (Pair i j))

{-# COMPLETE FromOneInUpTo :: FromOneInUpTo #-}
pattern FromOneInUpTo :: (FromOneInUpToAs f, KnownNetType t) => Channel -> Channel -> Layer -> (f t)
pattern FromOneInUpTo i j k <- (preview fromOneInUpToPrism -> Just (MkFromOneInUpTo k (Pair i j)))
  where FromOneInUpTo i j k = review fromOneInUpToPrism (MkFromOneInUpTo k (Pair i j))

toOneInUpTo_ :: (ToOneInUpToAs f, KnownNetType t) => ToOneInUpTo t -> (f t)
toOneInUpTo_ = review toOneInUpToPrism

fromOneInUpTo_ :: (FromOneInUpToAs f, KnownNetType t) => FromOneInUpTo t -> (f t)
fromOneInUpTo_ = review fromOneInUpToPrism

instance KnownNetType t => Show (ToOneInUpTo t) where
        showsPrec p (ToOneInUpTo i j k) = showParen (p >= 11) $
            showsTypeRep (typeRep (Proxy :: Proxy t)) .
            showString "ToOneInUpTo " .
            showsPrec 11 i . showChar ' ' .
            showsPrec 11 j . showChar ' ' . 
            showsPrec 11 k

instance KnownNetType t => Show (FromOneInUpTo t) where
        showsPrec p (FromOneInUpTo i j k) = showParen (p >= 11) $
            showsTypeRep (typeRep (Proxy :: Proxy t)) .
            showString "FromOneInUpTo " .
            showsPrec 11 i . showChar ' ' .
            showsPrec 11 j . showChar ' ' . 
            showsPrec 11 k

-- | Literal of 'ToOneInUpTo' with positive polarity.
toOneInUpToLit :: (KnownNetType t, ToOneInUpToAs f) => Channel -> Channel -> Layer -> Lit (f t)
toOneInUpToLit i j k = PosLit $ ToOneInUpTo i j k

-- | Literal of 'FromOneInUpTo' with positive polarity.
fromOneInUpToLit :: (KnownNetType t, FromOneInUpToAs f) => Channel -> Channel -> Layer -> Lit (f t)
fromOneInUpToLit i j k = PosLit $ FromOneInUpTo i j k

-- | Literal of 'ToOneInUpTo' with positive polarity.
-- Returns Nothing if the two channel arguments are equal.
toOneInUpToLit' :: (KnownNetType t, ToOneInUpToAs f) => Channel -> Channel -> Layer -> Maybe (Lit (f t))
toOneInUpToLit' i j k
    | i == j    = Nothing
    | otherwise = Just $ toOneInUpToLit i j k

-- | Literal of 'FromOneInUpTo' with positive polarity.
-- Returns Nothing if the two channel arguments are equal.
fromOneInUpToLit' :: (KnownNetType t, FromOneInUpToAs f) => Channel -> Channel -> Layer -> Maybe (Lit (f t))
fromOneInUpToLit' i j k
    | i == j    = Nothing
    | otherwise = Just $ fromOneInUpToLit i j k

-- #############################################################################
-- #############################################################################

data ToBetweenBefore 
    = MkToBetweenBefore Layer (Pair 'Unordered 'NoDuplicates Channel)
    deriving stock (Generic, Eq, Ord, Ix)
    deriving Enum via FiniteEnumeration ToBetweenBefore
    deriving Bounded via Generically ToBetweenBefore

class ToBetweenBeforeAs a where
    toBetwBefPrism :: Prism' a ToBetweenBefore

instance ToBetweenBeforeAs ToBetweenBefore where
    toBetwBefPrism :: Prism' ToBetweenBefore ToBetweenBefore
    toBetwBefPrism = prism id Right

{-# COMPLETE ToBetweenBefore :: ToBetweenBefore #-}
pattern ToBetweenBefore :: (ToBetweenBeforeAs a) => Channel -> Channel -> Layer -> a
pattern ToBetweenBefore i j k <- (preview toBetwBefPrism -> Just (MkToBetweenBefore k (Pair i j)))
  where ToBetweenBefore i j k = toBetweenBefore_ $ MkToBetweenBefore k (Pair i j)

toBetweenBefore_ :: ToBetweenBeforeAs a => ToBetweenBefore -> a
toBetweenBefore_ = review toBetwBefPrism 

instance Show ToBetweenBefore where
    showsPrec p (ToBetweenBefore i j k) = showParen (p >= 11) $
        showString "ToBetweenBefore " .
        showsPrec 11 i . showChar ' ' .
        showsPrec 11 j . showChar ' ' . 
        showsPrec 11 k

-- | Literal of 'ToBetweenBefore' with positive polarity.
toBetweenBeforeLit :: ToBetweenBeforeAs a => Channel -> Channel -> Layer -> Lit a
toBetweenBeforeLit i j k = PosLit $ ToBetweenBefore i j k


-- #############################################################################
-- #############################################################################

data ViaWrongTwist
    = MkViaWrongTwist Layer (Pair 'Ordered 'NoDuplicates Channel)
    deriving stock (Generic, Eq, Ord, Ix)
    deriving Enum via FiniteEnumeration ViaWrongTwist
    deriving Bounded via Generically ViaWrongTwist

class ViaWrongTwistAs a where
    viaWrongTwistPrism :: Prism' a ViaWrongTwist

instance ViaWrongTwistAs ViaWrongTwist where
    viaWrongTwistPrism :: Prism' ViaWrongTwist ViaWrongTwist
    viaWrongTwistPrism = prism id Right

{-# COMPLETE ViaWrongTwist :: ViaWrongTwist #-}
pattern ViaWrongTwist :: ViaWrongTwistAs a => Channel -> Channel -> Layer -> a
pattern ViaWrongTwist i j k <- (preview viaWrongTwistPrism -> Just (MkViaWrongTwist k (Pair i j)))
  where ViaWrongTwist i j k = viaWrongTwist_ $ MkViaWrongTwist k (Pair i j)

viaWrongTwist_ :: ViaWrongTwistAs a => ViaWrongTwist -> a
viaWrongTwist_ = review viaWrongTwistPrism 

instance Show ViaWrongTwist where
    showsPrec p (ViaWrongTwist i j k) = showParen (p >= 11) $
        showString "ViaWrongTwist " .
        showsPrec 11 i . showChar ' ' .
        showsPrec 11 j . showChar ' ' . 
        showsPrec 11 k

-- | Literal of 'ViaWrongTwist' with positive polarity.
viaWrongTwistLit :: ViaWrongTwistAs a => Channel -> Channel -> Layer -> Lit a
viaWrongTwistLit i j k = PosLit $ ViaWrongTwist i j k


-- #############################################################################
-- #############################################################################

data SortedRel (t :: NetworkType)
    = MkSortedRel
        (Pair 'Ordered 'WithDuplicates BetweenLayers) -- replace with BetweenLayers BetweenLayers?
        (Pair (AreGateChannelsOrdered t) 'NoDuplicates Channel)
    deriving stock (Generic, Eq, Ord, Ix)
    deriving Enum via FiniteEnumeration (SortedRel t)
    deriving Bounded via Generically (SortedRel t)

class SortedRelAs f where
    sortedRelPrism :: Prism' (f t) (SortedRel t)

instance SortedRelAs SortedRel where
    sortedRelPrism = prism id Right

{-# COMPLETE SortedRel :: SortedRel #-}
pattern SortedRel :: (SortedRelAs f, KnownNetType t) => Value -> Value -> f t
pattern SortedRel v1 v2 <- (preview sortedRelPrism >=> pure . srToVals -> Just (v1,v2))
  where SortedRel (Value i k) (Value j l) = sortedRel_ $ MkSortedRel (Pair k l) (Pair i j)

srToVals :: KnownNetType t => SortedRel t -> (Value, Value)
srToVals (MkSortedRel (Pair k l) (Pair i j)) = ((Value i k), (Value j l))

sortedRel_ :: (SortedRelAs f, KnownNetType t) => SortedRel t -> f t
sortedRel_ = review sortedRelPrism 

instance KnownNetType t => Show (SortedRel t) where
    showsPrec p (SortedRel v1 v2) = showParen (p >= 11) $
        showsTypeRep (typeRep (Proxy :: Proxy t)) .
        showString "SortedRel " .
        showsPrec 11 v1 . showChar ' ' . showsPrec 11 v2

-- | Literal of 'SortedRel' with positive polarity.
sortedRelLit :: (KnownNetType t, SortedRelAs f) => Value -> Value -> Lit (f t)
sortedRelLit v1 v2 = PosLit $ SortedRel v1 v2


-- #############################################################################
-- #############################################################################

-- | @Value i k@ creates a variable \(v_i^k\) representing the value on
-- channel \(i\) between layers \(k\) and \(k+1\).
data Value = MkValue BetweenLayers Channel
    deriving stock (Generic, Eq, Ord, Ix)
    deriving Enum via (FiniteEnumeration Value)
    deriving Bounded via (Generically Value)

class ValueAs v where
    valuePrism :: Prism' v Value

instance ValueAs Value where
    valuePrism = prism id Right

{-# COMPLETE Value :: Value #-}
pattern Value :: ValueAs v => Channel -> BetweenLayers -> v
pattern Value i k <- (preview valuePrism -> Just (MkValue k i))
  where Value i k = value_ (MkValue k i)

value_ :: ValueAs v => Value -> v
value_ = review valuePrism

instance Show Value where
    showsPrec p (Value i k) = showParen (p >= 11) $
        showString "Value " . showsPrec 11 i . showChar ' ' . showsPrec 11 k 

inputValues :: ValueAs v => [v]
inputValues = map value_ $
    range
        ( Value firstChannel beforeFirstLayer
        , Value lastChannel  beforeFirstLayer)

outputValues :: ValueAs v => [v]
outputValues = map value_ $
    range
        ( Value firstChannel afterLastLayer
        , Value lastChannel  afterLastLayer)

-- | Literal of 'Value' with positive polarity.
valueLit :: ValueAs v => Channel -> BetweenLayers -> Lit v
valueLit i k = PosLit (Value i k)


-- #############################################################################
-- #############################################################################

-- data NetRunNr (n :: Nat) where
--     MkNetRun :: KnownNat n -> Value -> NetRunNr n

-- data NetRun where
--     MkNetRun :: forall n. KnownNat n -> Value -> NetRun n

data NetworkSynthesis t
    = GU (GateOrUnused t)
    | TOIUT Layer (Pair.AbsDiffGT1 (AreGateChannelsOrdered t) Channel)
    | FOIUT Layer (Pair.AbsDiffGT1 (AreGateChannelsOrdered t) Channel)
    | ToBetwBef ToBetweenBefore
    | VWT ViaWrongTwist
    | SR (SortedRel t)
    | Val Word32 Value
    deriving stock (Generic, Eq, Ord)
    deriving Enum via (FiniteEnumeration (NetworkSynthesis t))

{-# COMPLETE SortedRel, ToOneInUpTo, FromOneInUpTo, ToBetweenBefore, ViaWrongTwist, Value, Gate, Unused :: NetworkSynthesis #-}
{-# COMPLETE SortedRel, ToOneInUpTo, FromOneInUpTo, ToBetweenBefore, ViaWrongTwist, Value, GateOrUnused :: NetworkSynthesis #-}

instance GateOrUnusedAs NetworkSynthesis where
    gateOrUnusedPrism :: Prism' (NetworkSynthesis t) (GateOrUnused t)
    gateOrUnusedPrism = prism constructGateOrUnused matchGateOrUnused 
      where
        constructGateOrUnused :: GateOrUnused t -> NetworkSynthesis t
        constructGateOrUnused = GU
        matchGateOrUnused :: NetworkSynthesis t -> Either (NetworkSynthesis t) (GateOrUnused t)
        matchGateOrUnused ns = case ns of
            GU gu -> Right gu
            _     -> Left ns

instance GateAs NetworkSynthesis where
    gatePrism :: KnownNetType t => Prism' (NetworkSynthesis t) (Gate t)
    gatePrism = gateOrUnusedPrism % gatePrism

instance KnownNetType t => UnusedAs (NetworkSynthesis t) where
    unusedPrism :: Prism' (NetworkSynthesis t) Unused
    unusedPrism = gateOrUnusedPrism % unusedPrism 

instance ToOneInUpToAs NetworkSynthesis where
    toOneInUpToPrism :: forall t. KnownNetType t => Prism' (NetworkSynthesis t) (ToOneInUpTo t)
    toOneInUpToPrism = prism constructToOneInUpTo matchToOneInUpTo
      where
        constructToOneInUpTo :: ToOneInUpTo t -> NetworkSynthesis t
        constructToOneInUpTo (ToOneInUpTo i j k) 
            | areAdjacent i j = Gate i j k
            | otherwise        = TOIUT k (Pair.AbsDiffGT1 i j)
        matchToOneInUpTo :: NetworkSynthesis t -> Either (NetworkSynthesis t) (ToOneInUpTo t)
        matchToOneInUpTo ns = case ns of
            Gate i j k | areAdjacent i j   -> Right $ ToOneInUpTo i j k
            TOIUT k (Pair.AbsDiffGT1 i j ) -> Right $ ToOneInUpTo i j k
            _     -> Left ns
            
instance FromOneInUpToAs NetworkSynthesis where
    fromOneInUpToPrism :: forall t. KnownNetType t => Prism' (NetworkSynthesis t) (FromOneInUpTo t)
    fromOneInUpToPrism = prism constructFromOneInUpTo matchFromOneInUpTo
      where
        constructFromOneInUpTo :: FromOneInUpTo t -> NetworkSynthesis t
        constructFromOneInUpTo (FromOneInUpTo i j k)
            | areAdjacent i j = Gate i j k
            | otherwise        = FOIUT k (Pair.AbsDiffGT1 i j)
        matchFromOneInUpTo :: NetworkSynthesis t -> Either (NetworkSynthesis t) (FromOneInUpTo t)
        matchFromOneInUpTo ns = case ns of
            Gate i j k | areAdjacent i j   -> Right $ FromOneInUpTo i j k
            FOIUT k (Pair.AbsDiffGT1 i j ) -> Right $ FromOneInUpTo i j k
            _     -> Left ns

instance ToBetweenBeforeAs (NetworkSynthesis t) where
    toBetwBefPrism :: Prism' (NetworkSynthesis t) ToBetweenBefore
    toBetwBefPrism = prism constructToBetwBef matchToBetwBef 
      where
        constructToBetwBef :: ToBetweenBefore -> NetworkSynthesis t
        constructToBetwBef = ToBetwBef
        matchToBetwBef :: NetworkSynthesis t -> Either (NetworkSynthesis t) ToBetweenBefore
        matchToBetwBef ns = case ns of
            ToBetwBef tbb -> Right tbb
            _     -> Left ns

instance ViaWrongTwistAs (NetworkSynthesis t) where
    viaWrongTwistPrism :: Prism' (NetworkSynthesis t) ViaWrongTwist
    viaWrongTwistPrism = prism constructViaWrongTwist matchViaWrongTwist 
      where
        constructViaWrongTwist :: ViaWrongTwist -> NetworkSynthesis t
        constructViaWrongTwist = VWT
        matchViaWrongTwist :: NetworkSynthesis t -> Either (NetworkSynthesis t) ViaWrongTwist
        matchViaWrongTwist ns = case ns of
            VWT vwt -> Right vwt
            _     -> Left ns

instance SortedRelAs NetworkSynthesis where
    sortedRelPrism :: Prism' (NetworkSynthesis t) (SortedRel t)
    sortedRelPrism = prism constructSortedRel matchSortedRel 
      where
        constructSortedRel :: SortedRel t -> NetworkSynthesis t
        constructSortedRel = SR
        matchSortedRel :: NetworkSynthesis t -> Either (NetworkSynthesis t) (SortedRel t)
        matchSortedRel ns = case ns of
            SR sr -> Right sr
            _     -> Left ns

instance ValueAs (Word32 -> NetworkSynthesis t) where
    valuePrism :: Prism' (Word32 -> NetworkSynthesis t) Value
    valuePrism = prism constructOffVal matchOffVal
      where
        constructOffVal :: Value -> Word32 -> NetworkSynthesis t
        constructOffVal = flip Val
        matchOffVal :: (Word32 -> NetworkSynthesis t) -> Either (Word32 -> NetworkSynthesis t) Value
        matchOffVal mkOffVal = case mkOffVal 0 of
            Val _off v -> Right v
            _          -> Left mkOffVal



-- pattern OffsetValue :: Word32 -> Value -> NetworkSynthesis t
-- pattern OffsetValue
-- instance 

{-# COMPLETE GU, ToOneInUpTo, FromOneInUpTo, Val :: NetworkSynthesis #-}

instance KnownNetType t => Show (NetworkSynthesis t) where
    showsPrec p ns = showParen (p >= 11) $ case ns of
        GU gu -> showString "GateOrUnused_ " . showsPrec 11 gu
        ToOneInUpTo i j k -> showString "ToOneInUpTo_ " . showsPrec 11 (ToOneInUpTo i j k :: ToOneInUpTo t)
        FromOneInUpTo i j k -> showString "FromOneInUpTo_ " . showsPrec 11 (FromOneInUpTo i j k :: FromOneInUpTo t)
        ToBetwBef tbb -> showString "ToBetweenBefore_ " . showsPrec 11 tbb
        VWT vwt -> showString "ViaWrongTwist_ " . showsPrec 11 vwt
        SR sr -> showString "SortedRel_ " . showsPrec 11 sr
        Val cexIdx val -> showString "Value_ " . showsPrec 11 cexIdx . showChar ' ' . showsPrec 11 val

instance KnownNetType t => Dimacs (NetworkSynthesis t) where
    toDIMACS ns = case ns of
        Val dimacsOffset val -> fromIntegral dimacsOffset + toDIMACS (Var (Val 0 val :: NetworkSynthesis t))
        _          -> toDIMACS (Var ns)


-- #############################################################################
-- #############################################################################

newtype CexRun = CexRun Value
    deriving newtype (Eq, Ord, Bounded, Enum)
    deriving Dimacs via (Var CexRun)

instance Show CexRun where
    showsPrec p (CexRun value) = showParen (p >= 11) $
        showString "CexRun " . showsPrec 11 value 

{-# COMPLETE Value :: CexRun #-}

instance ValueAs CexRun where
    valuePrism :: Prism' CexRun Value
    valuePrism = prism CexRun matchCexRun
      where
        matchCexRun :: CexRun -> Either CexRun Value
        matchCexRun (CexRun v) = Right v
