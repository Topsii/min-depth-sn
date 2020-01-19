{-# language DeriveGeneric #-}
{-# language FlexibleContexts #-}

{-# LANGUAGE PatternSynonyms #-}

module MinDepthSN.Data.Unused
    ( Unused(Unused)
    , unusedLit
    ) where

import Generic.Data
import GHC.Generics (Generic)
import SAT.IPASIR (AsVar(..), Lit, lit)
import MinDepthSN.Data.Size

-- | @Unused i k@ creates a variable \(unused_i^k\) indicating a
-- channel \(i\) is not used by any comparator gate in layer \(k\).
data Unused = MkUnused { layer :: Layer, channel :: Channel }
    deriving (Eq, Generic, Ord)

{-# COMPLETE Unused #-}
pattern Unused :: Channel -> Layer -> Unused
pattern Unused i k = MkUnused k i

instance Show Unused where
    show (Unused i k) = "Unused " ++ show i ++ " " ++ show k

instance Bounded Unused where
    minBound = gminBound
    maxBound = gmaxBound

instance Enum Unused where
    toEnum = gtoFiniteEnum
    fromEnum = gfromFiniteEnum
    enumFrom = gfiniteEnumFrom
    enumFromThen = gfiniteEnumFromThen
    enumFromTo = gfiniteEnumFromTo
    enumFromThenTo = gfiniteEnumFromThenTo

-- | Literal of 'Unused' with positive polarity.
unusedLit :: AsVar v Unused => Channel -> Layer -> Lit v
unusedLit i k = lit (Unused i k)