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
    | Function ContVContVAA

type Ident = String
-- Value -> Answer
data ContVA = Ap1 Expr Env ContVA
    | Ap2 ContVContVAA ContVA

-- Value -> ContVA -> Answer
data ContVContVAA = Lam Ident Expr Env


applyVA :: ContVA -> Value -> Answer
applyVA (Ap1 e2 env k) v = let (Function f) = v in
    eval e2 env (Ap2 f k)
applyVA (Ap2 f k) v = applyVAContVAA f v k

applyVAContVAA :: ContVContVAA -> Value -> ContVA -> Answer
applyVAContVAA (Lam x e env) arg k = eval e (define env x arg) k

eval :: Expr -> Env -> ContVA -> Answer
eval (Number n) env k = applyVA k (IntVal n)
eval (Variable x) env k = applyVA k (find env x)
eval (Apply e1 [e2]) env k = eval e1 env (Ap1 e2 env k)
eval (Lambda [x] e) env k = applyVA k (Function (Lam x e env))
