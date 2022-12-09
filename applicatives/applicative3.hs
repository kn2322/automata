{-# LANGUAGE GADTs, KindSignatures, RankNTypes, ScopedTypeVariables #-}
import GHC.Exts (Constraint)

data NAF :: (* -> Constraint) -> (* -> *) -> * -> * where
    Pure :: a -> NAF c f a
    Ap :: c z => NAF c f (z -> a) -> f z -> NAF c f a

instance Functor (NAF c f) where
    fmap f (Pure x) = Pure (f x) -- hom law
    fmap f (Ap g x) = Ap (fmap (f .) g) x
    {- composition law
    u <*> (v <*> w) = pure (.) <*> u <*> v <*> w
    So
    pure f <*> (g <*> x) = pure (.) <*> pure f <*> g <*> x
    = pure (f .) <*> g <*> x
    = fmap (f .) g <*> x
    -}
instance Applicative (NAF c f) where
    pure = Pure
    (Pure f) <*> (Pure x) = Pure (f x)
    f <*> (Pure x) = Pure (\g -> g x) <*> f
    f <*> (Ap g x) = Ap (Pure (.) <*> f <*> g) x

liftNAF :: c a => f a -> NAF c f a
liftNAF x = Ap (Pure id) x

{-foldNAF :: forall c f a g. (forall b . b -> g b) -> (forall z b . c b => g (z -> b) -> f z -> g b) -> NAF c f a -> g a
foldNAF pur app = foldNAF'
    where
        foldNAF' :: forall q . NAF c f q -> g q
        foldNAF' (Pure b) = pur b
        foldNAF' (Ap n tx) = app (foldNAF' n) tx-}

foldNAF :: forall r t c. (forall x. x -> r x) ->
    (forall y z. c y => r (y -> z) -> t y -> r z) ->
    forall a. NAF c t a -> r a
foldNAF pur app = foldNAF'
    where
        foldNAF' :: forall b. NAF c t b -> r b
        foldNAF' (Pure b) = pur b
        foldNAF' (Ap n tx) = app (foldNAF' n) tx
