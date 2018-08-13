{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module MinDepthSN.Data.GeneralizedGateOrUnused
    ( GeneralizedGateOrUnused(GeneralizedGateOrUnused, GeneralizedGate, Unused, ..)
    , GeneralizedGate
    , Unused
    ) where

import Enumerate.Enum.Valid (Validatable, isValid)
import MinDepthSN.Data.Size (Channel, Layer)
import MinDepthSN.Data.GeneralizedGate (GeneralizedGate)
import MinDepthSN.Data.Unused (Unused)
import qualified MinDepthSN.Data.GeneralizedGate as GeneralizedGate
import qualified MinDepthSN.Data.Unused as Unused

data GeneralizedGateOrUnused 
    = GeneralizedGate_ GeneralizedGate
    | Unused_ Unused
    deriving (Eq, Ord)

{-# COMPLETE  GeneralizedGate,  Unused #-}
{-# COMPLETE  GeneralizedGate, Unused_ #-}
{-# COMPLETE GeneralizedGate_,  Unused #-}
{-# COMPLETE GeneralizedGate_, Unused_ #-}
{-# COMPLETE GeneralizedGateOrUnused   #-}

pattern GeneralizedGate :: Channel -> Channel -> Layer -> GeneralizedGateOrUnused
pattern GeneralizedGate i j k = 
    GeneralizedGate_ (GeneralizedGate.GeneralizedGate i j k)

pattern Unused :: Channel -> Layer -> GeneralizedGateOrUnused
pattern Unused i k = Unused_ (Unused.Unused i k)

pattern GeneralizedGateOrUnused :: Channel -> Channel -> Layer -> GeneralizedGateOrUnused
pattern GeneralizedGateOrUnused i j k <- (matchGateOrUnused -> (i, j, k)) where
    GeneralizedGateOrUnused i j k
        | i /= j = GeneralizedGate i j k
        | otherwise = Unused i k

matchGateOrUnused :: GeneralizedGateOrUnused -> (Channel, Channel, Layer)
matchGateOrUnused v = case v of
    GeneralizedGate i j k -> (i, j, k)
    Unused i k -> (i, i, k)

instance Show GeneralizedGateOrUnused where
    show gu = case gu of
        (GeneralizedGate_ g) -> show g
        (Unused_ u)          -> show u

instance Validatable GeneralizedGateOrUnused where
    isValid gu = case gu of
        GeneralizedGate_ g -> isValid g
        Unused_ u          -> isValid u

instance Bounded GeneralizedGateOrUnused where
    minBound = min (GeneralizedGate_ minBound) (Unused_ minBound)
    maxBound = max (GeneralizedGate_ maxBound) (Unused_ maxBound)

compOffset :: Int
compOffset = 0

unusedOffset :: Int
unusedOffset = fromEnum (GeneralizedGate_ maxBound) + 1

instance Enum GeneralizedGateOrUnused where

    toEnum i
        | i < 0 = error $ "toEnum (GeneralizedGateOrUnused): negative argument "
            ++ show i
        | i <= fromEnum (GeneralizedGate_ maxBound) = GeneralizedGate_ $ 
            toEnum (i -   compOffset)
        | i <= fromEnum (         Unused_ maxBound) = Unused_ $
            toEnum (i - unusedOffset)
        | otherwise = error $ "toEnum (GeneralizedGateOrUnused):"
            ++ "argument " ++ show i ++ "exceeds maxBound"

    fromEnum gu = case gu of
        GeneralizedGate_ g ->   compOffset + fromEnum g
        Unused_ u          -> unusedOffset + fromEnum u