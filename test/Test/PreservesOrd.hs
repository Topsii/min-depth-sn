
module Test.PreservesOrd where

import Data.List (sort)

data PreservesOrder = PreservesOrd | ViolatesOrd


-- sorts a list iff the first argument is ViolatesOrd
sortIfOrdViolated :: Ord a => PreservesOrder -> [a] -> [a]
sortIfOrdViolated ord = case ord of
    PreservesOrd -> id
    ViolatesOrd  -> sort