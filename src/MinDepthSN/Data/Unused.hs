{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}

module MinDepthSN.Data.Unused
    ( Unused(Unused)
    ) where

import Data.Ord (comparing)
import Data.Monoid ((<>))
import GHC.Generics (Generic)
import Enumerate (Enumerable)
import Enumerate.Enum.Valid (Validatable, isValid)
import MinDepthSN.Data.Size

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
