import Control.Arrow

fix :: (a -> a) -> a
fix f = f (fix f)

lpArr ::((b, d) -> (c, d)) -> (b -> c)
lpArr f b = let (c, d) = f (b, d) in c--fst $ fix (\(c, d) -> f (b, d))


fact :: Int -> Int
fact = lpArr factLoop--lpArr factLoop

factK :: (Int -> Int) -> (Int -> Int)
factK k n = if n == 0 then 1 else n * k (n-1)

type Functional = (Int -> Int) -> (Int -> Int)

factLoop :: (Int, Int -> Int) -> (Int, Int -> Int)
factLoop (n, k) = (k n, factK k)
