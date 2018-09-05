{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# language FlexibleContexts #-}

module MinDepthSN.Data.Unused
    ( Unused(Unused)
    , unusedLit
    ) where

import Data.Ord (comparing)
import Data.Monoid ((<>))
import GHC.Generics (Generic)
import Enumerate (Enumerable)
import Enumerate.Enum.Valid (Validatable, isValid)
import SAT.IPASIR.EnumVarSolver (AsVar(..), Lit, lit)
import MinDepthSN.Data.Size

-- | @Unused i k@ creates a variable \(unused_i^k\) indicating a
-- channel \(i\) is not used by any comparator gate in layer \(k\).
data Unused = Unused { channel :: Channel, layer :: Layer }
    deriving (Eq, Generic, Enumerable)

instance Show Unused where
    show (Unused i k) = "Unused " ++ show i ++ " " ++ show k

instance Ord Unused where
    compare = comparing layer <> comparing channel

instance Bounded Unused where
    minBound = toEnum 0
    maxBound = toEnum (n*d - 1)

instance Validatable Unused where
    isValid = const True

instance Enum Unused where
    toEnum int = Unused (toEnum i) (toEnum k)
      where
        (k,i) = int `quotRem` n
    fromEnum (Unused i k) = fromEnum k * n + fromEnum i

-- | Literal of 'Unused' with positive polarity.
unusedLit :: AsVar v Unused => Channel -> Layer -> Lit v
unusedLit i k = lit (Unused i k)