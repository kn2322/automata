import Control.Arrow (Arrow(..), first, second, (***), arr, (&&&), (>>>))
data Exp  = Var String | Add Exp Exp | If Exp Exp Exp 
data Val = Num Int | Bl Bool deriving Show
type Env = [(String, Val)]

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

switch :: Arrow y => y a Bool -> y a b -> y a b -> y a b
switch c x y = c &&& (x &&& y) >>> arr (\(u, (l, r)) -> if u then l else r)
