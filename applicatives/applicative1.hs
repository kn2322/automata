{-# LANGUAGE GADTs #-}
data NAF f a where
    Pure :: a -> NAF f a
    Ap :: NAF f (z -> a) -> f z -> NAF f a

instance Functor (NAF f) where
    fmap f (Pure x) = Pure (f x) -- hom law
    fmap f (Ap g x) = Ap (fmap (f .) g) x
    {- composition law
    u <*> (v <*> w) = pure (.) <*> u <*> v <*> w
    So
    pure f <*> (g <*> x) = pure (.) <*> pure f <*> g <*> x
    = pure (f .) <*> g <*> x
    = fmap (f .) g <*> x
    -}
instance Applicative (NAF f) where
    pure = Pure
    (Pure f) <*> (Pure x) = Pure (f x)
    (Pure f) <*> (Ap g x) = Ap (Pure (f .) <*> g) x
    (Ap f x) <*> Pure y = Ap (Pure (\g z -> g z y) <*> f) x
    (Ap f x) <*> (Ap g y) = Ap ((Ap (Pure (\h z i w -> h z (i w)) <*> f) x) <*> g) y
