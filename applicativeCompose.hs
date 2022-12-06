-- proving that applicative composition is associative
{-
TODO:
    * Calculate the applicative normal form of RHS
    * Compare LHS and RHS and check they are equal
    * Are applicative normal forms unique?
    * Implement free applicative correctly (what does that even eman?)
    * Write a function to automate reduction to applicative normal form using laws involving (.)
    * Why can't the type checker infer the type of the compositions of compositions?
    * Is there a way to get the expression tree of lhs with types annotated?
-}
{-# LANGUAGE GADTs #-}
infixl 4 <.>
(<.>) :: Applicative f => f (b -> c) -> f (a -> b) -> f (a -> c)
f <.> g = pure (.) <*> f <*> g

-- u :: Applicative f => f (a -> b)
-- u = undefined
-- v :: Applicative f => f (b -> c)
-- v = undefined
-- w :: Applicative f => f (c -> d)
-- w = undefined

data Free a where
    Pure :: a -> Free a
    Zap :: Free (z -> a) -> Free z -> Free a

instance Functor Free where
    fmap f (Pure x) = Pure (f x)
    --fmap f (Zap u v) = Pure (f .) `Zap` u `Zap` v
    fmap f (Zap u v) = (fmap (f .) u) `Zap` v

instance Applicative Free where -- definitely wrong
    pure = Pure
    (Pure f) <*> x = fmap f x
    (Zap f x) <*> (Pure y) = Pure ($ y) <*> (f <*> x)
    (Zap f x) <*> (Zap g y) = (f <*> x) <*> (g <*> y)

u :: Free (a -> b)
u = undefined
v :: Free (b -> c)
v = undefined
w :: Free (c -> d)
w = undefined

lhs :: Free (a -> c)
lhs = (u <.> v) <.> w
lhs1 :: Free (a -> c)
lhs1 = (pure (.) <*> u <*> v) <.> w
lhs2 :: Free (a -> c)
lhs2 = pure (.) <*> (pure (.) <*> u <*> v) <*> w
lhs3 :: Free (a -> c)
lhs3 = pure ((.) $ (.)) <*> (pure (.) <*> u) <*> v <*> w
lhs4 :: Free (a -> c)
lhs4 = pure (((.) $ ((.) $ (.))) $ (.)) <*> u <*> v <*> w
--lhs5 :: Free (a -> c)
--lhs5 = pure _ <*> u <*> v <*> w -- for some reason the type inference doesn't help here
