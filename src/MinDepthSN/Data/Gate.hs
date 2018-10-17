{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language ScopedTypeVariables #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses #-}
{-# language DerivingStrategies #-}

{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}

{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language TypeOperators #-}
{-# language InstanceSigs #-}
{-# language GADTs #-}
{-# language StandaloneDeriving #-}
-- {-# language UndecidableInstances #-}
{-# language TypeFamilies #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -w #-}



module MinDepthSN.Data.Gate where

import Data.Ord (comparing)
import Data.Monoid ((<>))
import GHC.Generics (Generic)
import Enumerate
    ( Enumerable
    , enumerated
    , boundedEnumerated
    , cardinality
    , boundedCardinality
    )
import Enumerate.Enum.Valid (Validatable, Valid(..), isValid)
import SAT.IPASIR (AsVar(..), Lit, lit)
import MinDepthSN.Data.Size (Channel, Layer)

import GHC.TypeNats
import Data.Proxy
import Numeric.Natural

import MinDepthSN.Data.Combinatorics2.CombinationNoRepetition
import MinDepthSN.Data.Combinatorics2.VariationNoRepetition

import Generic.Data
import Control.Arrow ((&&&))

-- data Choose (n :: Nat) (k :: Nat) where
--     ChooseMax :: Choose (n-1) (k-1) -> Choose n k
--     DontChooseMax :: Choose (n-1) k -> Choose n k
--     ChooseNothing :: Choose 0 0

instance Show a => Show (Choose a k) where
    show c = case c of
        ChooseNone  -> ""
        Choose a c' -> case c' of
            ChooseNone -> show a
            _          -> show a ++ ", " ++ show c'

pattern Chose :: Ord a => a -> Choose a k -> Choose a (k+1)
pattern Chose a c <- Choose a c
  where
    Chose a c = case c of
        ChooseNone -> Choose a c
        (Choose a' _) | a < a' -> Choose a c
                      | otherwise -> error "Chose"

data Choose a (k :: Nat) where
    Choose :: a -> Choose a k -> Choose a (k+1)
    ChooseNone :: Choose a 0

-- data Choose (n :: Nat) (k :: Nat) where
--     MkChoose :: forall n k x. KnownNat x => Choose (n-x) (k-1) -> Choose n k
--     MkChooseNone :: Choose n 0

-- choose :: Natural -> Choose n 1
-- choose x = case someNatVal x of
--     SomeNat (_ :: Proxy x) -> (MkChoose :: KnownNat x => Choose (n-x) (k-1) -> Choose n k) MkChooseNone

-- instance  (KnownNat n, KnownNat k) => Show (Choose n k) where
--     show _ = show (natVal (Proxy :: Proxy n)) ++ " choose " ++ show (natVal (Proxy :: Proxy k))

data Cmp (o :: SortingOrder) where
    StdCmp :: CombinationNoRepetition Channel -> Cmp 'Standard
    GenCmp :: (Channel, Channel) -> Cmp 'Generalized


    
data SortingOrder = Standard | Generalized
    deriving (Eq, Show)

class SortOrder a where
    sortOrder :: a -> SortingOrder

instance SortOrder StandardGate where
    sortOrder = const Standard

instance SortOrder GeneralizedGate where
    sortOrder = const Generalized


-- | @Gate i j k@ creates a variable \(g_{i,j}^k\) representing a
-- comparator gate where \(i\) and \(j\) are the channels and \(k\) is the layer.
data Gate f = MkGate { layer :: Layer, channels :: f Channel }
    deriving Generic

-- deriving instance Eq (f Channel) => Eq (Gate f)
-- deriving instance Ord (f Channel) => Ord (Gate f)
-- deriving instance Bounded (f Channel) => Bounded (Gate f)
-- deriving instance Show (f Channel) => Show (Gate f)
-- instance (Bounded (f Channel), Enum (f Channel)) => Enum (Gate f) where
--     toEnum = gtoFiniteEnum
--     fromEnum = gfromFiniteEnum
--     enumFrom = gfiniteEnumFrom
--     enumFromThen = gfiniteEnumFromThen
--     enumFromTo = gfiniteEnumFromTo
--     enumFromThenTo = gfiniteEnumFromThenTo

data family TwoNoRepetition (f :: * -> *)
data instance TwoNoRepetition CombinationNoRepetition = CombinationNoRepetitio
data instance TwoNoRepetition VariationNoRepetition = VariationNoRepetitio

data family TwoNoRepetition3 (o :: SortingOrder)
data instance TwoNoRepetition3 'Standard = CombinationNoRepetiti
data instance TwoNoRepetition3 'Generalized = VariationNoRepetiti

data family TwoNoRepetition4 (o :: SortingOrder) (a :: *)
data instance TwoNoRepetition4 'Standard a = CombinationNoRepetit a
data instance TwoNoRepetition4 'Generalized a = VariationNoRepetit a

data family Gt (o :: SortingOrder) :: * 
data instance Gt 'Standard = CombinationNoRepetition2 Channel
data instance Gt 'Generalized = VariationNoRepetition2 Channel

-- data family Gt2 (f :: * -> *) :: *
-- data instance Gt2 CombinationNoRepetition =  Standard
-- data instance Gt2 VariationNoRepetition  =  Generalized

data family TwoNoRepetition' :: * -> SortingOrder -> *
newtype instance TwoNoRepetition' a 'Standard = Mk1 (CombinationNoRepetition a)
newtype instance TwoNoRepetition' a 'Generalized = Mk2 (VariationNoRepetition a)

data family TwoChannels :: SortingOrder -> *
newtype instance TwoChannels 'Standard = Mk3 (CombinationNoRepetition Channel)
newtype instance TwoChannels 'Generalized = Mk4 (VariationNoRepetition Channel)

instance Enum (TwoChannels o) where
    toEnum n = undefined
    fromEnum = undefined

data TwoDifferent (o :: SortingOrder) a where
    A :: CombinationNoRepetition a -> TwoDifferent 'Standard a
    B :: VariationNoRepetition a -> TwoDifferent 'Generalized a


data Gatee (o :: SortingOrder) =
    MkGatee { layer2 :: Layer, channels2 :: TwoNoRepetition' Channel o }
    deriving Generic


data CombinationNoRepetition' a b = MkCombinationNoRepetition a b
    deriving (Eq, Ord, Show)
    
data VariationNoRepetition' a b = MkVariationNoRepetition a b
    deriving (Eq, Ord, Show)

-- data family TwoNoRepetition'' :: * -> * -> SortingOrder -> *
-- newtype instance TwoNoRepetition'' a b 'Standard = Mk1' (CombinationNoRepetition' a b)
-- newtype instance TwoNoRepetition'' a b 'Generalized = Mk2' (VariationNoRepetition' a b)

data family TwoNoRepetition'' :: SortingOrder -> * -> * -> *
newtype instance TwoNoRepetition'' 'Standard a b  = Mk1' (CombinationNoRepetition' a b)
newtype instance TwoNoRepetition'' 'Generalized a b = Mk2' (VariationNoRepetition' a b)

type family Gateee (o :: SortingOrder) where
    Gateee 'Standard = Gate CombinationNoRepetition
    Gateee 'Generalized = Gate VariationNoRepetition

-- instance Eq (Gateee o) where

-- newtype Gateeeo o = Gateeeo (Gateee o)
--     deriving Eq

-- deriving instance Eq (Gateeeo (o::SortingOrder))

-- class Two f where
--     data Elem f
--     two :: Elem f -> Elem f -> f (Elem f) -- fromTuple?
--     first :: f (Elem f) -> Elem f
--     second :: f (Elem f) -> Elem f

-- instance (Ord a, Show a) => Two CombinationNoRepetition where
--     two = CombinationNoRepetition
--     first (CombinationNoRepetition x _) = x
--     second (CombinationNoRepetition _ x) = x

-- instance (Eq a, Show a) => Two VariationNoRepetition where
--     two = VariationNoRepetition
--     first (VariationNoRepetition x _) = x
--     second (VariationNoRepetition _ x) = x

-- use a type family / data family instead?
-- somehow require f to be a data family in declaration of the Gate data type?
class Two f a where
    two :: a -> a -> f a -- fromTuple?
    first :: f a -> a
    second :: f a -> a

instance (Ord a, Show a) => Two CombinationNoRepetition a where
    two = CombinationNoRepetition
    first (CombinationNoRepetition x _) = x
    second (CombinationNoRepetition _ x) = x

instance (Eq a, Show a) => Two VariationNoRepetition a where
    two = VariationNoRepetition
    first (VariationNoRepetition x _) = x
    second (VariationNoRepetition _ x) = x

{-# COMPLETE Gate #-}
pattern Gate :: Two f Channel => Channel -> Channel -> Layer -> Gate f
pattern Gate i j k <- MkGate k (first &&& second -> (i,j))
  where
    Gate i j k = MkGate k (two i j)
-- pattern Gate :: Channel -> Channel -> Layer -> Gate f
-- pattern Gate i j k <- (undefined -> (i, j, k))
--   where
--     Gate i j k = undefined

type StandardGate = Gate CombinationNoRepetition
type GeneralizedGate = Gate VariationNoRepetition


-- DataKinds creates a type identifier from this, that conflicts with the typesynonym StandardGate
-- pattern StandardGate :: Channel -> Channel -> Layer -> StandardGate
-- pattern StandardGate i j k = Gate i j k

-- DataKinds creates a type identifier from this, that conflicts with the typesynonym GeneralizedGate
-- pattern GeneralizedGate :: Channel -> Channel -> Layer -> GeneralizedGate
-- pattern GeneralizedGate i j k = Gate i j k


-- | Literal of 'Gate' with positive polarity.
gateLit 
    :: forall v f. (AsVar (v f) (Gate f), Two f Channel)
    => Channel -> Channel -> Layer -> Lit (v f)
gateLit i j k = lit (Gate i j k :: Gate f)

