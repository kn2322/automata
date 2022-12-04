-- My attempt of deriving the CEK machine from funmini

type Env = [(Ident, Value)]
find :: Env -> Ident -> Value
find env x = case lookup x env of
    Just v -> v
    Nothing -> error $ "Missing identifier " ++ x ++ " from environment."

define :: Env -> Ident -> Value -> Env
define env x v = (x, v) : env

--
data Expr = Number Int
    | Variable Ident
    | Apply Expr [Expr]
    | Lambda [Ident] Expr

data Value = IntVal Int
    | Function (Value -> Value)

type Ident = String

eval :: Expr -> Env -> Value
eval (Number n) env = IntVal n
eval (Variable x) env = find env x
eval (Apply e1 [e2]) env = let (Function f) = eval e1 env in f (eval e2 env)
eval (Lambda [x] e) env = Function (\arg -> eval e (define env x arg))
