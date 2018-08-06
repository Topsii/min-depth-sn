{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module MinDepthSN.Data.GateOrUnused
    ( GateOrUnused(GateOrUnused, Gate, Unused, ..)
    , Gate
    , Unused
    ) where

import Enumerate.Enum.Valid (Validatable, isValid)
import MinDepthSN.Data.Size (Channel, Layer)
import MinDepthSN.Data.Gate (Gate)
import MinDepthSN.Data.Unused (Unused)
import qualified MinDepthSN.Data.Gate as Data
import qualified MinDepthSN.Data.Unused as Data

data GateOrUnused 
    = Gate_ Gate
    | Unused_ Unused
    deriving (Eq, Ord)

{-# COMPLETE  Gate,  Unused #-}
{-# COMPLETE  Gate, Unused_ #-}
{-# COMPLETE Gate_,  Unused #-}
{-# COMPLETE Gate_, Unused_ #-}
{-# COMPLETE GateOrUnused   #-}

pattern Gate :: Channel -> Channel -> Layer -> GateOrUnused
pattern Gate i j k = Gate_ (Data.Gate i j k)

pattern Unused :: Channel -> Layer -> GateOrUnused
pattern Unused i k = Unused_ (Data.Unused i k)

pattern GateOrUnused :: Channel -> Channel -> Layer -> GateOrUnused
pattern GateOrUnused i j k <- (matchGateOrUnused -> (i, j, k)) where
    GateOrUnused i j k
        | i > j = Gate j i k
        | i < j = Gate i j k
        | otherwise = Unused i k

matchGateOrUnused :: GateOrUnused -> (Channel, Channel, Layer)
matchGateOrUnused v = case v of
    Gate i j k -> (i, j, k)
    Unused i k -> (i, i, k)

instance Show GateOrUnused where
    show gu = case gu of
        (Gate_ g)   -> show g
        (Unused_ u) -> show u

instance Validatable GateOrUnused where
    isValid gu = case gu of
        Gate_ g   -> isValid g
        Unused_ u -> isValid u

instance Bounded GateOrUnused where
    minBound = min (Gate_ minBound) (Unused_ minBound)
    maxBound = max (Gate_ maxBound) (Unused_ maxBound)

compOffset :: Int
compOffset = 0

unusedOffset :: Int
unusedOffset = fromEnum (Gate_ maxBound) + 1

instance Enum GateOrUnused where

    toEnum i
        | i < 0 = error $ "toEnum (GateOrUnused): negative argument " ++ show i
        | i <= fromEnum (Gate_ maxBound)   = Gate_   $ toEnum (i - compOffset  )
        | i <= fromEnum (Unused_ maxBound) = Unused_ $ toEnum (i - unusedOffset)
        | otherwise = error $ "toEnum (GateOrUnused):"
            ++ "argument " ++ show i ++ "exceeds maxBound"

    fromEnum gu = case gu of
        Gate_ g   -> compOffset   + fromEnum g
        Unused_ u -> unusedOffset + fromEnum u