
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Data.Pair.OrderAndDup where

import Type.Reflection
import Data.Typeable (eqT)

data Order = Ordered | Unordered

data Duplicates = WithDuplicates | NoDuplicates

orderAndDup :: forall o d. (Typeable o, Typeable d) => OrderAndDup o d
orderAndDup = case eqT :: Maybe ('Unordered :~: o) of
    Just Refl -> case eqT :: Maybe ('NoDuplicates :~: d) of
        Just Refl -> UND
        Nothing   -> case eqT:: Maybe ('WithDuplicates :~: d) of
            Just Refl -> UWD
            Nothing   -> error "bad"
    Nothing   -> case eqT:: Maybe ('Ordered :~: o) of
        Just Refl -> case eqT :: Maybe ('NoDuplicates :~: d) of
            Just Refl -> OND
            Nothing   -> case eqT:: Maybe ('WithDuplicates :~: d) of
                Just Refl -> OWD
                Nothing   -> error "bad"
        Nothing   -> error "bad"

data OrderAndDup (o :: Order) (d :: Duplicates) where
    UND :: OrderAndDup 'Unordered 'NoDuplicates
    OND :: OrderAndDup 'Ordered   'NoDuplicates
    UWD :: OrderAndDup 'Unordered 'WithDuplicates
    OWD :: OrderAndDup 'Ordered   'WithDuplicates
