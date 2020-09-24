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
    , Unused
        ( Unused
        , Unused_
        )
    , UnusedAs()
    , unusedLit
    -- * Gate
    , Gate
        ( Gate
        , Gate_
        )
    , GateAs()
    , gateLit
    -- * GateOrUnused
    , GateOrUnused
        ( GateOrUnused
        , Gate_
        , Unused_
        , Unused
        )
    , GateOrUnusedAs(..)
    , gateOrUnusedLit
    -- * Value
    , Value
        ( Value
        , Value_
        )
    , ValueAs()
    , inputValues
    , outputValues
    , valueLit
    -- * NetworkSynthesis
    , NetworkSynthesis
        ( Gate
        , Gate_
        , Unused
        , Unused_
        , GateOrUnused
        , GateOrUnused_
        , Value
        , Value_
        )
    -- * CexRun
    , CexRun
        ( Value
        , Value_
        )
    ) where

import Data.Ix
import Generic.Data
-- import SAT.IPASIR (AsVar(..), Lit, lit)
import SAT.IPASIR (Var(..), Dimacs(..), Lit(PosLit))
import MinDepthSN.Data.Size

import Optics.Core ((%),  preview, prism, review, Prism' )


import Data.Pair
import Data.Proxy
import Data.Typeable ( showsTypeRep, typeRep )
import Data.Word (Word32)

import MinDepthSN.Data.NetworkType


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
  where Unused i k = review unusedPrism $ MkUnused k i

{-# COMPLETE Unused_ :: Unused #-}
pattern Unused_ :: UnusedAs u => Unused -> u
pattern Unused_ u <- (preview unusedPrism -> Just u)
  where Unused_ u = review unusedPrism u

instance Show Unused where
    showsPrec p (Unused i k) = showParen (p >= 11) $
        showString "Unused " . showsPrec 11 i . showChar ' ' . showsPrec 11 k 

-- | Literal of 'Unused' with positive polarity.
unusedLit :: UnusedAs u => Channel -> Layer -> Lit u
unusedLit i k = PosLit $ Unused i k


-- #############################################################################
-- #############################################################################

-- | @Gate i j k@ creates a variable \(g_{i,j}^k\) representing a
-- comparator gate where \(i\) and \(j\) are the channels and \(k\) is the layer.
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
    -- Will it be erased by specifying the signature here?
    gatePrism :: forall t. Prism' (Gate t) (Gate t)
    gatePrism = prism id Right

{-# COMPLETE Gate :: Gate #-}
pattern Gate :: (GateAs g, KnownNetType t) => Channel -> Channel -> Layer -> g t
pattern Gate i j k = Gate_ (MkGate k (Pair i j))

{-# COMPLETE Gate_ :: Gate #-}
pattern Gate_ :: (GateAs g, KnownNetType t) => Gate t -> g t
pattern Gate_ x <- (preview gatePrism -> Just x)
  where Gate_ x = review gatePrism x

instance KnownNetType t => Show (Gate t) where
    showsPrec p (Gate i j k) = showParen (p >= 11) $
        showsTypeRep (typeRep (Proxy :: Proxy t)) .
        showString "Gate " .
        showsPrec 11 i . showChar ' ' .
        showsPrec 11 j . showChar ' ' . 
        showsPrec 11 k

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
pattern GateOrUnused i j k = GateOrUnused_ (MkGateOrUnused k (Pair i j))

{-# COMPLETE GateOrUnused_ :: GateOrUnused #-}
pattern GateOrUnused_ :: (GateOrUnusedAs gu, KnownNetType t) => GateOrUnused t -> gu t
pattern GateOrUnused_ x <- (preview gateOrUnusedPrism -> Just x)
  where GateOrUnused_ x = review gateOrUnusedPrism x

{-# COMPLETE Gate, Unused :: GateOrUnused #-}
{-# COMPLETE Gate_, Unused :: GateOrUnused #-}
{-# COMPLETE Gate, Unused_ :: GateOrUnused #-}
{-# COMPLETE Gate_, Unused_ :: GateOrUnused #-}

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
    showsPrec p (Gate_ g)   = showParen (p >= 11) $ showString "Gate_ "   . showsPrec 11 g 
    showsPrec p (Unused_ u) = showParen (p >= 11) $ showString "Unused_ " . showsPrec 11 u

-- | Literal of 'GateOrUnused' with positive polarity.
gateOrUnusedLit :: (KnownNetType t, GateOrUnusedAs gu) => Channel -> Channel -> Layer -> Lit (gu t)
gateOrUnusedLit i j k = PosLit $ GateOrUnused i j k


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
pattern Value i k = Value_ (MkValue k i)

{-# COMPLETE Value_ :: Value #-}
pattern Value_ :: ValueAs v => Value -> v
pattern Value_ v <- (preview valuePrism -> Just v)
  where Value_ v = review valuePrism v

instance Show Value where
    showsPrec p (Value i k) = showParen (p >= 11) $
        showString "Value " . showsPrec 11 i . showChar ' ' . showsPrec 11 k 

inputValues :: ValueAs v => [v]
inputValues = map Value_ $
    range
        ( Value firstChannel beforeFirstLayer
        , Value lastChannel  beforeFirstLayer)

outputValues :: ValueAs v => [v]
outputValues = map Value_ $
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
    | Val Word32 Value
    deriving stock (Generic, Eq, Ord)
    deriving Enum via (FiniteEnumeration (NetworkSynthesis t))

{-# COMPLETE Value, Gate, Unused :: NetworkSynthesis #-}
{-# COMPLETE Value, Gate, Unused_ :: NetworkSynthesis #-}
{-# COMPLETE Value, Gate_, Unused :: NetworkSynthesis #-}
{-# COMPLETE Value, Gate_, Unused_ :: NetworkSynthesis #-}
{-# COMPLETE Value, GateOrUnused :: NetworkSynthesis #-}
{-# COMPLETE Value, GateOrUnused_ :: NetworkSynthesis #-}
{-# COMPLETE Value_, Gate, Unused :: NetworkSynthesis #-}
{-# COMPLETE Value_, Gate, Unused_ :: NetworkSynthesis #-}
{-# COMPLETE Value_, Gate_, Unused :: NetworkSynthesis #-}
{-# COMPLETE Value_, Gate_, Unused_ :: NetworkSynthesis #-}
{-# COMPLETE Value_, GateOrUnused :: NetworkSynthesis #-}
{-# COMPLETE Value_, GateOrUnused_ :: NetworkSynthesis #-}

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
{-# COMPLETE Value_ :: CexRun #-}

instance ValueAs CexRun where
    valuePrism :: Prism' CexRun Value
    valuePrism = prism CexRun matchCexRun
      where
        matchCexRun :: CexRun -> Either CexRun Value
        matchCexRun (CexRun v) = Right v
