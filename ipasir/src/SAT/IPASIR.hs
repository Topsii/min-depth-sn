module SAT.IPASIR where

--import SAT.IPASIR.Bindings


--solveUnder assumptionClauses ::





{-
class Literal f where
    toDIMACS :: Integral i => f a -> i
    fromDIMACS :: Integral i => i -> f a

instance Literal Negated where
    toDIMACS = undefined
    fromDIMACS = undefined

instance Literal Var where
    toDIMACS = undefined
    fromDIMACS = undefined

newtype Negated a = Neg (Var a)

newtype Var a = Var a
-}


{-
data Var a = Var Bool a | Nil
    deriving (Show)

instance Enum a => Enum (Var a) where
    toEnum n
        | n < 0 = Var False (toEnum $ n + 1)
        | n > 0 = Var True  (toEnum $ n - 1)
        | otherwise = Nil
    
    fromEnum var = case var of
        Var _ x -> fromEnum x + 1
        Nil     -> negate $ fromEnum v




instance Enum a => Num (Var a) where
    (+) = error "(+): Cannot add two values of type Var."
    (*) = error "(+): Cannot multiply two values of type Var."
    abs =
    signum var = case arv of
        Neg v = Neg $ signum v
        Var v = 1
    fromInteger 
    negate var = case var of
        Var b _ -> Var (not b) _
        Nil     -> Nil

instance Enum a => Real (Var a) where

instance Enum a => Integral (Var a) where
-}

 












{-
instance Enum a => Num (Var a) where
    (+) = error "(+): Cannot add two values of type Var."
    (*) = error "(+): Cannot multiply two values of type Var."
    abs =
    signum v = case v of
        Neg v = Neg $ signum v
        Var v = 1
    fromInteger 
    negate = Neg

instance Enum a => Real (Var a) where

instance Enum a => Integral (Var a) where


 
data Var a
    = Neg (Var a)
    | Var a
    deriving (Show)

instance Enum a => Enum (Var a) where
    toEnum n
        | n < 0     = Neg . Var . toEnum . negate $ n + 1
        | otherwise =       Var . toEnum          $ n - 1
    
    fromEnum var = case var of
        Var x -> fromEnum x + 1
        Neg v -> negate $ fromEnum v-}