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
    | Function (Value -> (Value -> Answer) -> Answer)

type Ident = String

eval :: Expr -> Env -> (Value -> Answer) -> Answer
eval (Number n) env k = k $ IntVal n
eval (Variable x) env k = k $ find env x
eval (Apply e1 [e2]) env k = eval e1 env (\(Function f) ->
    eval e2 env (\v ->
    f v k))
eval (Lambda [x] e) env k = k $ Function (\arg k2 -> eval e (define env x arg) k2)
