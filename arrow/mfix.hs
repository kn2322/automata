import Control.Monad.Fix
-- monad fix for factorial

factK :: (Int -> Int) -> [Int -> Int]
factK k = [k, \n -> -k n]
