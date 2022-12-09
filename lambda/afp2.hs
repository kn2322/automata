{-# LANGUAGE GADTs, DataKinds, TypeOperators, KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

type family Cat (a :: [*]) (b :: [*]) :: [*] where
    Cat '[] ys = ys
    Cat (x ': xs) ys = x ': (Cat xs ys)

data HList :: [*] -> * where
    Nil :: HList '[]
    Cons :: a -> HList as -> HList (a ': as)

data Pos :: * -> [*] -> * where
    Top :: Pos a (a ': as)
    Pop :: Pos a as -> Pos a (a ': as)

look :: Pos b as -> HList as -> b
look Top (Cons x xs) = x
look (Pop p) (Cons x xs) = look p xs

data Term :: [*] -> * -> * where
    App :: Term ctx (s -> t) -> Term ctx s -> Term ctx t
    Lam :: Term (s ': ctx) t -> Term ctx (s -> t)
    Var :: Pos s ctx -> Term ctx s

eval :: Term ctx s -> HList ctx -> s
eval (Var pos) ctx = look pos ctx
eval (App f x) ctx = (eval f ctx) (eval x ctx)
eval (Lam e) ctx = \x -> eval e (Cons x ctx)

beta :: Term (s ': ctx) t -> Term ctx' s -> Term (Cat ctx ctx') t
beta f e = undefined
