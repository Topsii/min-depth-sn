{-# language ScopedTypeVariables #-}

module Data.Enum where

-- | > preceding x == [ minBound .. pred x ]
preceding :: forall a. (Bounded a, Enum a) => a -> [a]
preceding x = map toEnum [ fromEnum (minBound :: a) .. fromEnum x - 1 ]

-- | > succeeding a == [ succ x .. maxBound ]
succeeding :: forall a. (Bounded a, Enum a) => a -> [a]
succeeding x = map toEnum [ fromEnum x + 1 .. fromEnum (maxBound :: a) ]

-- | > between from to == [ succ from .. pred to ]
between :: (Bounded a, Enum a) => a -> a -> [a]
between from to = map toEnum [ fromEnum from + 1 .. fromEnum to - 1 ]

-- | Default implementation for 'enumFrom'.
boundedEnumFrom :: forall a. (Bounded a, Enum a) => a -> [a]
boundedEnumFrom x1 = map toEnum [ i_x1 .. i_max ]
  where
    i_x1, i_max :: Int
    i_x1 = fromEnum x1
    i_max = fromEnum (maxBound :: a)

-- | Default implementation for 'enumFromThen'.
boundedEnumFromThen :: forall a. (Bounded a, Enum a) => a -> a -> [a]
boundedEnumFromThen x1 x2 = map toEnum [ i_x1, i_x2 .. i_bound ]
  where
    i_x1, i_x2, i_bound :: Int
    i_x1 = fromEnum x1
    i_x2 = fromEnum x2
    i_bound = fromEnum bound
    bound :: a
    bound | i_x1 < i_x2 = minBound
          | otherwise   = maxBound