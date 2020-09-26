{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# language KindSignatures #-}

module MinDepthSN.Data.NetworkType where


import Data.Typeable 
import Data.Pair

data NetworkType = Standard | Generalized

type family AreGateChannelsOrdered (t :: NetworkType) :: Order where
    AreGateChannelsOrdered 'Standard    = 'Unordered
    AreGateChannelsOrdered 'Generalized = 'Ordered

class ( Typeable t
      , Typeable (AreGateChannelsOrdered t)
      ) => KnownNetType (t :: NetworkType) where

instance KnownNetType 'Standard
instance KnownNetType 'Generalized