{-# language GADTs #-}
{-# language TypeOperators #-}
{-# language ConstraintKinds #-}
{-# language StandaloneDeriving #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module MinDepthSN.Variables.Test where

import Data.Finite
import GHC.TypeLits

type a == b = (a <= b, b <= a)

data WindowedValue z w o n d where
    WindowedValue :: ((z + w + o) == n) => Finite w -> Finite d -> WindowedValue z w o n d

deriving instance ((z + w + o) == n) => Show (WindowedValue z w o n d)