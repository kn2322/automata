-- My attempt of deriving the CEK machine from funmini

type Env = [(Ident, Value)]
find :: Env -> Ident -> Value
find env x = case lookup x env of
    Just v -> v
    Nothing -> error $ "Missing identifier " ++ x ++ " from environment."

define :: Env -> Ident -> Value -> Env
define env x v = (x, v) : env

defargs :: Env -> [Ident] -> [Value] -> Env
defargs env xs vs = foldr (\(x, v) e' -> define e' x v) env (zip xs vs)
--
data Expr = Number Int
    | Variable Ident
    | Apply Expr [Expr]
    | Lambda [Ident] Expr

data Value = IntVal Int
    | Function ([Value] -> Value)

type Ident = String

eval :: Expr -> Env -> Value
eval (Number n) env = IntVal n
eval (Variable x) env = find env x
eval (Apply e es) env = apply (eval e env) [eval f env | f <- es]
eval (Lambda xs e) env = Function (\args -> eval e (defargs env xs args))

apply :: Value -> [Value] -> Value
apply f args = case f of
    Function g -> g args
    _ -> error "Applying a non-function."
