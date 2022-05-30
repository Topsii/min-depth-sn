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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

{-# language TypeFamilies #-}

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
    -- * Max / Min
    , Max
        ( Max
        )
    , Min ( Min )
    , MaxAs(..)
    , MinAs(..)
    , max_
    , min_
    , maxLit
    , minLit
    , maxLit'
    , minLit'
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
import Data.Typeable ( showsTypeRep, typeRep, Typeable )
import Data.Word (Word32)

import MinDepthSN.Data.NetworkType
import Control.Monad ((>=>))
import Data.Kind


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

data DeterminateChannel = DeterminateMin | DeterminateMax
    deriving stock (Show, Eq, Ord, Bounded, Enum, Ix)

data GateRange (s :: DeterminateChannel) (t :: NetworkType) = MkGateRange Layer (Pair (AreGateChannelsOrdered t) 'NoDuplicates Channel)
    deriving stock (Generic, Eq, Ord, Ix)
    deriving Enum via (FiniteEnumeration (GateRange s t))
    deriving Bounded via (Generically (GateRange s t))

-- class MinAs f where
--     minPrism :: KnownNetType t => Prism' (f t) (Min t)
-- class (GRAs a s t ~ a) => GateRangeAs a s t where
--     type GRAs a s t :: Type
--     gateRangePrism :: Prism' (GRAs a s t) (GateRange s t)

class GateRangeAs f s where
    gateRangePrism :: Prism' (f t) (GateRange s t)

-- instance GateRangeAs (GateRange s t) s t where
--     type GRAs (GateRange s t) s t = GateRange s t
--     gateRangePrism = prism id Right

instance GateRangeAs (GateRange t) s t where
    gateRangePrism = prism id Right

{-# COMPLETE GateRange :: GateRange #-}
-- pattern GateRange :: (GateRangeAs a s t, KnownNetType t) => Proxy s -> Proxy t -> Channel -> Channel -> Layer -> a
-- pattern GateRange ps pt i j k <- (preview gateRangePrism -> Just (MkGateRange k (Pair i j)))
--     where GateRange ps pt i j k = review gateRangePrism (MkGateRange k (Pair i j))

pattern GateRange :: forall a s t. (GateRangeAs a s t, KnownNetType t) => Channel -> Channel -> Layer -> GRAs a s t
pattern GateRange i j k <- (preview gateRangePrism -> Just (MkGateRange k (Pair i j) :: GateRange s t))
    where GateRange i j k = review gateRangePrism (MkGateRange k (Pair i j))

gateRange_ :: (GateRangeAs a s t) => GateRange s t -> a
gateRange_ = review gateRangePrism

-- | Literal of 'Max' with positive polarity.
gateRange :: forall a s t. (KnownNetType t, GateRangeAs a s t) => Channel -> Channel -> Layer -> Lit a
gateRange i j k = PosLit $ GateRange @a @s @t i j k

-- | Literal of 'Max' with positive polarity.
-- Returns Nothing if the two channel arguments are equal.
gateRange' :: (KnownNetType t, GateRangeAs a s t) => Channel -> Channel -> Layer -> Maybe (Lit a)
gateRange' i j k
    | i == j    = Nothing
    | otherwise = Just $ gateRange i j k

instance (Typeable s, KnownNetType t) => Show (GateRange s t) where
        showsPrec p (GateRange i j k :: GRAs a s t) = showParen (p >= 11) $
            showsTypeRep (typeRep (Proxy :: Proxy t)) .
            showsTypeRep (typeRep (Proxy :: Proxy s)) .
            showsPrec 11 i . showChar ' ' .
            showsPrec 11 j . showChar ' ' . 
            showsPrec 11 k
    

{- | @Max i j k@ creates a variable \(t_{i,j}^k\) with \( i \neq j \) 
representing the fact that there is a gate with min channel \(l\) and max
channel \(j\) in layer \(k\) where \(l\) is in the range from \(j\) up
to \(i\) exclusive \(j\) and inclusive \(i\), i.e. 
  
  * \(t_{i,j}^k\) with \( i < j \) indicates there is some min-max gate \( g_{l,j}^k\) with \(i \le l < j \)
  in layer \(k\)

  * \(t_{i,j}^k\) with \( i > j \) indicates there is some max-min gate \( g_{l,j}^k\) with \(i \ge l > j \)
  in layer \(k\)

  See 'Min' for the motivation of this variable.
-}
data Max (t :: NetworkType) = MkMax Layer (Pair (AreGateChannelsOrdered t) 'NoDuplicates Channel)
    deriving stock (Generic, Eq, Ord, Ix)
    deriving Enum via (FiniteEnumeration (Max t))
    deriving Bounded via (Generically (Max t))


{- | @Min i j k@ creates a variable \(f_{i,j}^k\) with \( i \neq j \) 
representing the fact that there is a gate with min channel \(i\) and max
channel \(l\) in layer \(k\) where \(l\) is in the range from \(i\) up
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
data Min (t :: NetworkType) = MkMin Layer (Pair (AreGateChannelsOrdered t) 'NoDuplicates Channel)
    deriving stock (Generic, Eq, Ord, Ix)
    deriving Enum via (FiniteEnumeration (Min t))
    deriving Bounded via (Generically (Min t))

class MaxAs f where
    maxPrism :: KnownNetType t => Prism' (f t) (Max t)

class MinAs f where
    minPrism :: KnownNetType t => Prism' (f t) (Min t)

instance MaxAs Max where
    maxPrism = prism id Right

instance MinAs Min where
    minPrism = prism id Right

{-# COMPLETE Max :: Max #-}
pattern Max :: (MaxAs f, KnownNetType t) => Channel -> Channel -> Layer -> f t
pattern Max i j k <- (preview maxPrism -> Just (MkMax k (Pair i j)))
  where Max i j k = review maxPrism (MkMax k (Pair i j))

{-# COMPLETE Min :: Min #-}
pattern Min :: (MinAs f, KnownNetType t) => Channel -> Channel -> Layer -> f t
pattern Min i j k <- (preview minPrism -> Just (MkMin k (Pair i j)))
  where Min i j k = review minPrism (MkMin k (Pair i j))

max_ :: (MaxAs f, KnownNetType t) => Max t -> f t
max_ = review maxPrism

min_ :: (MinAs f, KnownNetType t) => Min t -> f t
min_ = review minPrism

instance KnownNetType t => Show (Max t) where
        showsPrec p (Max i j k) = showParen (p >= 11) $
            showsTypeRep (typeRep (Proxy :: Proxy t)) .
            showString "Max " .
            showsPrec 11 i . showChar ' ' .
            showsPrec 11 j . showChar ' ' . 
            showsPrec 11 k

instance KnownNetType t => Show (Min t) where
        showsPrec p (Min i j k) = showParen (p >= 11) $
            showsTypeRep (typeRep (Proxy :: Proxy t)) .
            showString "Min " .
            showsPrec 11 i . showChar ' ' .
            showsPrec 11 j . showChar ' ' . 
            showsPrec 11 k

-- | Literal of 'Max' with positive polarity.
maxLit :: (KnownNetType t, MaxAs f) => Channel -> Channel -> Layer -> Lit (f t)
maxLit i j k = PosLit $ Max i j k

-- | Literal of 'Min' with positive polarity.
minLit :: (KnownNetType t, MinAs f) => Channel -> Channel -> Layer -> Lit (f t)
minLit i j k = PosLit $ Min i j k

-- | Literal of 'Max' with positive polarity.
-- Returns Nothing if the two channel arguments are equal.
maxLit' :: (KnownNetType t, MaxAs f) => Channel -> Channel -> Layer -> Maybe (Lit (f t))
maxLit' i j k
    | i == j    = Nothing
    | otherwise = Just $ maxLit i j k

-- | Literal of 'Min' with positive polarity.
-- Returns Nothing if the two channel arguments are equal.
minLit' :: (KnownNetType t, MinAs f) => Channel -> Channel -> Layer -> Maybe (Lit (f t))
minLit' i j k
    | i == j    = Nothing
    | otherwise = Just $ minLit i j k

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
    | GR DeterminateChannel Layer (Pair.AbsDiffGT1 (AreGateChannelsOrdered t) Channel)
    | ToBetwBef ToBetweenBefore
    | VWT ViaWrongTwist
    | SR (SortedRel t)
    | Val Word32 Value
    deriving stock (Generic, Eq, Ord)
    deriving Enum via (FiniteEnumeration (NetworkSynthesis t))

{-# COMPLETE SortedRel, Max, Min, ToBetweenBefore, ViaWrongTwist, Value, Gate, Unused :: NetworkSynthesis #-}
{-# COMPLETE SortedRel, Max, Min, ToBetweenBefore, ViaWrongTwist, Value, GateOrUnused :: NetworkSynthesis #-}

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

instance GateRangeAs (NetworkSynthesis t) s t where
    type GRAs (NetworkSynthesis t) s t = NetworkSynthesis t
    gateRangePrism :: forall t. KnownNetType t => Prism' (NetworkSynthesis t) (Max t)
    gateRangePrism = prism constructMax matchMax
        where
        constructMax :: Max t -> NetworkSynthesis t
        constructMax (Max i j k) 
            | areAdjacent i j = Gate i j k
            | otherwise        = TOIUT k (Pair.AbsDiffGT1 i j)
        matchMax :: NetworkSynthesis t -> Either (NetworkSynthesis t) (Max t)
        matchMax ns = case ns of
            Gate i j k | areAdjacent i j   -> Right $ Max i j k
            TOIUT k (Pair.AbsDiffGT1 i j ) -> Right $ Max i j k
            _     -> Left ns

instance MaxAs NetworkSynthesis where
    maxPrism :: forall t. KnownNetType t => Prism' (NetworkSynthesis t) (Max t)
    maxPrism = prism constructMax matchMax
      where
        constructMax :: Max t -> NetworkSynthesis t
        constructMax (Max i j k) 
            | areAdjacent i j = Gate i j k
            | otherwise        = TOIUT k (Pair.AbsDiffGT1 i j)
        matchMax :: NetworkSynthesis t -> Either (NetworkSynthesis t) (Max t)
        matchMax ns = case ns of
            Gate i j k | areAdjacent i j   -> Right $ Max i j k
            TOIUT k (Pair.AbsDiffGT1 i j ) -> Right $ Max i j k
            _     -> Left ns
            
instance MinAs NetworkSynthesis where
    minPrism :: forall t. KnownNetType t => Prism' (NetworkSynthesis t) (Min t)
    minPrism = prism constructMin matchMin
      where
        constructMin :: Min t -> NetworkSynthesis t
        constructMin (Min i j k)
            | areAdjacent i j = Gate i j k
            | otherwise        = FOIUT k (Pair.AbsDiffGT1 i j)
        matchMin :: NetworkSynthesis t -> Either (NetworkSynthesis t) (Min t)
        matchMin ns = case ns of
            Gate i j k | areAdjacent i j   -> Right $ Min i j k
            FOIUT k (Pair.AbsDiffGT1 i j ) -> Right $ Min i j k
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

{-# COMPLETE GU, Max, Min, Val :: NetworkSynthesis #-}

instance KnownNetType t => Show (NetworkSynthesis t) where
    showsPrec p ns = showParen (p >= 11) $ case ns of
        GU gu -> showString "GateOrUnused_ " . showsPrec 11 gu
        Max i j k -> showString "Max_ " . showsPrec 11 (Max i j k :: Max t)
        Min i j k -> showString "Min_ " . showsPrec 11 (Min i j k :: Min t)
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
