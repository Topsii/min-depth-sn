module Lib
    ( someFunc
    , triangularNumber
    ) where


import SAT.IPASIR.DimacsSolver
import Foreign.C.Types (CInt)



-- | Determines the n-th triangular number for a given non-negative integer n.
--    triangularNumber n == sum [0..n-1]
triangularNumber :: Int -> Int
triangularNumber n | n < 0 = error "triangularNumber: negative n was passed."
triangularNumber n = (n * (n - 1)) `div` 2

cnf :: [[CInt]]
cnf = [[-1,2,5],[-5,4],[2,3],[1,2,3,4,5],[4,-2],[-2,1],[1,2,4,5]]


someFunc :: IO ()
someFunc = putStrLn ipasirSignature >> print (runSolver $ do
    r <- solveCNF cnf
    vals <- assignments [1..5] 
    return (r,vals))

    
--ExceptT Alternative/MonadPlus instance to collect cex for cegis?

--runSolver