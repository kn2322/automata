-- In complete syntactic mode :)
import Data.Maybe (isNothing)
import Text.Pretty.Simple (pPrint)

data Expr = Number Int | Variable Ident |
    Apply Expr Expr | Lambda Ident Expr deriving Show
data Value = Closure Ident Expr Env | IntVal Int | Succ | Pred deriving Show
--type ControlStack = [Control]
data Control = ContArg Expr Env | ContApp Value deriving Show
type Env = [(Ident, Value)]
find :: Env -> Ident -> Value
find env x = let (Just v) = lookup x env in v
--define env x v = (x, v) : env
type Ident = String

data Config = Eval Expr Env [Control] | Continue [Control] Value | Fail deriving Show

run :: Config -> Config
run (Eval (Number n) env ks) = Continue ks (IntVal n)
run (Eval (Variable x) env ks) = Continue ks (find env x)
run (Eval (Lambda x e) env ks) = Continue ks (Closure x e env)
run (Eval (Apply e1 e2) env ks) = Eval e1 env (ContArg e2 env : ks)
run (Continue (ContArg e env : ks) v) = Eval e env (ContApp v : ks)
run (Continue (ContApp (Closure x e env) : ks) v) = Eval e ((x, v) : env) ks

run (Continue (Pred : ks) (IntVal n)) = Continue ks (n-1)
run (Continue (Succ : ks) (IntVal n)) = Continue ks (n+1)
run (Continue [] v) = Continue [] v -- success state
run c = Fail -- no absorbing states
--error ("No reduction rule from configuration " ++ show c)

extractMaybe :: Config -> Maybe Value
extractMaybe (Continue [] v) = Just v
extractMaybe _ = Nothing

calculate :: Config -> [Config]
calculate c = takeWhile1 (isNothing . extractMaybe) (iterate run c)

takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 p [] = []
takeWhile1 p (x:xs) = case p x of
    True -> x : takeWhile1 p xs
    False -> x : []

init_env = [("succ", Succ), ("pred", Pred)]

plus3 :: Config -> Config
plus3 (Eval e env ks) = Eval (foldNat e (Apply (Variable "succ")) 3) env ks

foldNat :: a -> (a -> a) -> Int -> a
foldNat z s n = if n == 0 then z
    else s (foldNat z s (n-1))

e = Apply
        (Lambda "x" (Variable "x"))
        (Apply
            (Lambda "y" (Variable "y"))
            (Number 42)
        )

t = Eval e [] []
main = pPrint (calculate t)

{-

*Main> pPrint (calculate t)
[ Eval
    ( Apply
        ( Lambda "x"
            ( Variable "x" )
        )
        ( Apply
            ( Lambda "y"
                ( Variable "y" )
            )
            ( Number 42 )
        )
    ) [] []
, Eval
    ( Lambda "x"
        ( Variable "x" )
    ) []
    [ ContArg
        ( Apply
            ( Lambda "y"
                ( Variable "y" )
            )
            ( Number 42 )
        ) []
    ]
, Continue
    [ ContArg
        ( Apply
            ( Lambda "y"
                ( Variable "y" )
            )
            ( Number 42 )
        ) []
    ]
    ( Closure "x"
        ( Variable "x" ) []
    )
, Eval
    ( Apply
        ( Lambda "y"
            ( Variable "y" )
        )
        ( Number 42 )
    ) []
    [ ContApp
        ( Closure "x"
            ( Variable "x" ) []
        )
    ]
, Eval
    ( Lambda "y"
        ( Variable "y" )
    ) []
    [ ContArg
        ( Number 42 ) []
    , ContApp
        ( Closure "x"
            ( Variable "x" ) []
        )
    ]
, Continue
    [ ContArg
        ( Number 42 ) []
    , ContApp
        ( Closure "x"
            ( Variable "x" ) []
        )
    ]
    ( Closure "y"
        ( Variable "y" ) []
    )
, Eval
    ( Number 42 ) []
    [ ContApp
        ( Closure "y"
            ( Variable "y" ) []
        )
    , ContApp
        ( Closure "x"
            ( Variable "x" ) []
        )
    ]
, Continue
    [ ContApp
        ( Closure "y"
            ( Variable "y" ) []
        )
    , ContApp
        ( Closure "x"
            ( Variable "x" ) []
        )
    ]
    ( IntVal 42 )
, Eval
    ( Variable "y" )
    [
        ( "y"
        , IntVal 42
        )
    ]
    [ ContApp
        ( Closure "x"
            ( Variable "x" ) []
        )
    ]
, Continue
    [ ContApp
        ( Closure "x"
            ( Variable "x" ) []
        )
    ]
    ( IntVal 42 )
, Eval
    ( Variable "x" )
    [
        ( "x"
        , IntVal 42
        )
    ] []
, Continue []
    ( IntVal 42 )
]
-}
