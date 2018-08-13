{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

{#context lib="ipasircryptominisat5" #}
#include "ipasir.h"

module SAT.IPASIR.Bindings
    ( Solver
    , runSolver
    , ipasirSignature
    , ipasirInit
    , ipasirAdd
    , ipasirAssume
    , ipasirSolve
    , ipasirVal
    , ipasirFailed
    ) where

import Control.Monad.ST.Unsafe (unsafeIOToST)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.Reader (ReaderT(..), runReaderT)

import Data.Word (Word8)
import Control.Applicative (liftA2)

import Foreign.Ptr (Ptr, castPtr)
import Foreign.ForeignPtr (ForeignPtr, FinalizerPtr, newForeignPtr, withForeignPtr)
import Foreign.Marshal.Array (peekArray0)
import Foreign.Marshal.Unsafe (unsafeLocalState)
import Foreign.C.Types (CInt)



--import Control.Monad.Indexed.State

--data SolverState = Input | Sat | Unsat

--newtype Solver :: * -> SolverState -> SolverState -> * -> * where
    --Solver ::  { unSolver :: ReaderT (SolverPtr s) (ST s) a } -> Solver s before after a
    --deriving (Functor, Applicative, Monad)




newtype Solver s a = Solver { unSolver :: ReaderT (SolverPtr s) (ST s) a }
    deriving (Functor, Applicative, Monad)

-- | Same Semigroup instance as 'Control.Monad.ST'
-- With GHC 8.6.1 this can be derived with the deriving via language extension.
instance Semigroup a => Semigroup (Solver s a) where
    (<>) = liftA2 (<>)

-- | Same Monoid instance as 'Control.Monad.ST'
-- With GHC 8.6.1 this can be derived with the deriving via language extension.
instance Monoid a => Monoid (Solver s a) where
    mempty = pure mempty

runSolver :: (forall s. Solver s {-Input j-} a) -> a
runSolver s = runST (ipasirInit >>= runReaderT (unSolver s))

newtype SolverPtr s = SolverPtr { fPtr :: ForeignPtr () }

foreign import ccall unsafe "SAT/IPASIR/Cryptominisat/C.chs.h &ipasir_release"
    ipasir_release :: FinalizerPtr ()

-- | Restricts the type of an IPASIR function call, that modifies the solver state. 
-- The solver state is given as a pointer. 
-- An aforementioned IPASIR function call should not be an arbitrary IO effect.
-- Instead only the solver-instance specific State Thread (ST) will be affected.
toSolverST :: (Ptr () -> IO a) -> Solver s {-i j-} a
toSolverST f = Solver . ReaderT $ \s -> unsafeIOToST $ withForeignPtr (fPtr s) f

-- | Same as 'toSolverST' except the converted function receives a 
-- literal as an additional argument.
toSolverSTwithLit :: (Ptr () -> CInt -> IO a) -> CInt -> Solver s {-i j-} a
toSolverSTwithLit f lit = toSolverST (`f` lit)

-- | The name and the version of the incremental SAT solving library.
ipasirSignature :: String
ipasirSignature = unsafeLocalState $ do
    ptr <- {#call unsafe ipasir_signature #}
    let iPtr = castPtr ptr :: Ptr Word8
    ints <- peekArray0 0 iPtr
    return $ map (toEnum . fromEnum) ints

ipasirInit :: ST s (SolverPtr s)
ipasirInit = unsafeIOToST $ do
    ptr <- {#call unsafe ipasir_init #}
    foreignPtr <- newForeignPtr ipasir_release ptr
    return $ SolverPtr foreignPtr

ipasirAdd :: CInt -> Solver s {-i Input-} ()
ipasirAdd = toSolverSTwithLit {#call unsafe ipasir_add #}

ipasirAssume :: CInt -> Solver s {-i Input-} ()
ipasirAssume = toSolverSTwithLit {#call unsafe ipasir_assume #}

ipasirSolve :: Solver s {-i Sat-} Bool
ipasirSolve = isSolved <$> toSolverST {#call unsafe ipasir_solve #}
  where
    isSolved :: CInt -> Bool
    isSolved i = case i of
        10 -> True
        20 -> False
        0  -> error $ "ipasirSolve returned 0, but ipasir_set_terminate was not called."
        x  -> error $ "ipasirSolve returned " ++ show x ++ ", but must be either 0, 10 or 20"

ipasirVal :: CInt -> Solver s {-Sat Sat-} Bool
ipasirVal lit = case lit of
    0 -> error "ipasirVal: Cannot check for variable 0."
    --_ -> (toEnum . fromIntegral) <$> toSolverSTwithLit {#call unsafe ipasir_val #} lit
    _ -> (> 0) <$> toSolverSTwithLit {#call unsafe ipasir_val #} lit

ipasirFailed :: CInt -> Solver s {-Unsat Unsat-} Bool
ipasirFailed lit = case lit of
    0 -> error "ipasirFailed: Cannot check for variable 0."
    _ -> (> 0) <$> toSolverSTwithLit {#call unsafe ipasir_failed #} lit

