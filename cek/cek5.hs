-- My attempt of deriving the CEK machine from funmini

type Env = [(Ident, Value)]
find :: Env -> Ident -> Value
find env x = case lookup x env of
    Just v -> v
    Nothing -> error $ "Missing identifier " ++ x ++ " from environment."

define :: Env -> Ident -> Value -> Env
define env x v = (x, v) : env

--

type Answer = String

data Expr = Number Int
    | Variable Ident
    | Apply Expr [Expr]
    | Lambda [Ident] Expr

data Value = IntVal Int
    | Closure Ident Expr Env

type Ident = String
-- Value -> Answer
data ContVA = Ap1 Expr Env ContVA -- (_(e), env) : k
    | Ap2 Value ContVA -- v(_) : k
    | Show Env


applyVA :: ContVA -> Value -> Answer
applyVA (Ap1 e2 env k) v = eval e2 env (Ap2 v k)
applyVA (Ap2 f k) v = applyValue f v k

applyValue :: Value -> Value -> ContVA -> Answer
applyValue (Closure x e env) arg k = eval e (define env x arg) k
applyValue _ _ _ = error "Applying a non-function"

eval :: Expr -> Env -> ContVA -> Answer
eval (Number n) env k = applyVA k (IntVal n)
eval (Variable x) env k = applyVA k (find env x)
eval (Apply e1 [e2]) env k = eval e1 env (Ap1 e2 env k)
eval (Lambda [x] e) env k = applyVA k (Closure x e env)
