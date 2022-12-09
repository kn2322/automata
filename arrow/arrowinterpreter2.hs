{-# LANGUAGE GADTs, RankNTypes #-}
import Control.Arrow (Arrow(..), first, second, (***), arr, (&&&), (>>>))
data Exp where
    Var :: String -> Exp
    Add :: Exp -> Exp -> Exp
    If :: Exp -> Exp -> Exp -> Exp
    App :: Exp -> Exp -> Exp
    Lam :: String -> Exp -> Exp

data Val where
    Num :: Int -> Val
    Bl :: Bool -> Val
    Fun :: (forall y. Arrow y => y Val Val) -> Val
type Env = [(String, Val)]

define :: Env -> String -> Val -> Env
define env x v = (x, v) : env

eval :: Arrow y => Exp -> y Env Val
eval (Var s) =
    let e = error ("Missing identifier " ++ s)
    in
    arr (maybe e id . lookup s)
eval (Add e1 e2) =
    let add ((Num x),(Num y)) = Num (x+y)
    in
    (eval e1 &&& eval e2) >>> arr add
eval (If e1 e2 e3) =
    switch
    (eval e1 >>> arr (\(Bl b) -> b))
    (eval e2)
    (eval e3)
eval (Lam x e) = arr $ \env -> Fun $
    arr (\v -> define env x v) >>> eval e
eval (App e1 e2) = eval e1 &&& eval e2 >>>

apply :: Arrow y => y a (y p q) -> y a p -> y a q
apply f x = error "Problem!!"

switch :: Arrow y => y a Bool -> y a b -> y a b -> y a b
switch c x y = c &&& (x &&& y) >>> arr (\(u, (l, r)) -> if u then l else r)
