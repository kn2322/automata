-- adding an alphabet: phrased as monoid homomorphisms
{-
TODO:
    * Find a nice proof of 'Induce induces a homomorphism'.
        * Current best requires induction to appeal to associativity of (<>)
        * Possible approach: general fact of mconcat (xs ++ ys) = mconcat xs <> mconcat ys
        * Backup plan: simply finish the proof
    * Prove N1
        * via proving run . determinise = run
            * via 'induce induces a homomorphism'
    * Prove N2

    Ideas:
    * Hierarchy of determinisation/lifting with Monad, Applicative, Functor, formalise.
    * Can we weaken the monad requirement on Speakable?
        * Clean up Speakable section
    * What is the space of list-algebras [Bool] -> Bool?
        * and, or, xor all seem to work
    * Describe the powerset constrained MFA, how is it related to the [] MFA?
    * Describe the product of LTS and NFA and prove its key property
    * Can we approach w-regular languages/Büchi automata this way?
    * Probability MFA in general has infinite state space due to limiting behaviour.
        * What is the space of algebras?
            * Mode of Prob Bool
            * Thresholding p -> Pr (p(1) ~ r) >= s
        * What are the languages accepted by the probability MFA with a choice of algebra?
    * Can we characterise Functors, Applicatives and Monads by the monoid actions they induce?
    * What on earth is an FFA and AFA?
    * Extended examples of automata in this framework used to model systems
        * in sore need
-}

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
import Control.Monad ((<=<))
import Data.List (inits)
import Data.Maybe (isJust)
--import Data.Void (Void)
--import Data.Functor.Compose (Compose)
import Control.Arrow ((***), (&&&))
--import Data.Functor.Identity (Identity, runIdentity)
import Data.Functor.Product (Product(..))
import Data.Functor.Sum (Sum(..))

class Monoid f => Act f a where
    act :: f -> (a -> a)
    -- commandment: f is a semigroup,
    -- law: act is a semigroup action
    -- act (a <> b) = act a . act b
    -- law: if act is a monoid
    -- act mempty = id

instance {-# OVERLAPPING #-} Semigroup (a -> a) where -- overlapping to override prelude Semigroup b => Semigroup (a -> b)
    (<>) = (.)

instance Applicative f => Semigroup (f (a -> a)) where
    f <> g = (.) <$> f <*> g

instance {-# OVERLAPPING #-} Monad m => Semigroup (a -> m a) where
    (<>) = (<=<)

instance {-# OVERLAPPING #-} Monoid (a -> a) where
    mempty = id

instance Applicative f => Monoid (f (a -> a)) where
    mempty = pure id

instance {-# OVERLAPPING #-} Monad m => Monoid (a -> m a) where
    mempty = return

instance Act (a -> a) a where
    act = id

instance Functor f => Act (a -> a) (f a) where
    act = fmap

instance Applicative f => Act (f (a -> a)) (f a) where
    act = (<*>)

instance Monad m => Act (a -> m a) (m a) where
    act = (=<<)

newtype Label a = Label {satisfies :: a -> Bool}

-- by the universal property of free monoids, induce
-- gives the unique hom. from Free(s) -> f
-- note Free(s) = [s] as monoids
induce :: Monoid f => (s -> f) -> ([s] -> f)
induce v = foldr (\x xs -> v x <> xs) mempty

-- alternative definition which may be way cuter algebraically speaking
-- induce v = mconcat . map v

type Automata s f a = (s -> f, a)
run :: Act f a => Automata s f a -> [s] -> a
run (v, x) w = act (induce v w) x

run1 :: Act f a => Automata s f a -> [s] -> [a]
run1 (v, x) w = [run (v, x) w' | w' <- inits w]

lang :: Act f a => Automata s f a -> Label a -> Label [s]
lang aut accept = Label (satisfies accept . run aut)

--- Section 3: relating automata
type Autonomous f a = Automata () f a
type Deterministic a = Automata () (a -> a) a
type DFA s a = Automata s (a -> a) a
type MFA s m a = Automata s (a -> m a) (m a) -- Monadeterministic finite automata
type AFA s f a = Automata s (f (a -> a)) (f a) -- Applicative finite automata
type FFA s f a = Automata s (a -> a) (f a) -- Functorial finite automata

type NFA s a = MFA s [] a

--lift :: Monad m => Automata s (a -> a) a -> Automata s (a -> m a) (m a
lift :: Monad m => DFA s a -> MFA s m a
lift = (\v a x -> return (v a x)) *** return

liftAp :: Applicative f => DFA s a -> AFA s f a
liftAp = (\v a -> pure (v a)) *** pure

--determinise :: Monad m => Automata s (a -> m a) (m a) -> Automata s (m a -> m a) (m a)

-- determinise :: NFA s a -> DFA s [a]
determinise :: Monad m => MFA s m a -> DFA s (m a)
determinise = (\v a xm -> v a =<< xm) *** id

determiniseAFA :: Applicative f => AFA s f a -> DFA s (f a)
determiniseAFA = (\v a xf -> (v a) <*> xf) *** id

determiniseFFA :: Functor f => FFA s f a -> DFA s (f a)
determiniseFFA = (\v a -> fmap (v a)) *** id

-- there's a compositional hierarchy going up with lift, going down with determinise. How can we express that elegantly?

--- Section 4: products and things

prod :: Automata s f a -> Automata s g b -> Automata s (f, g) (a, b)
-- Note well-defined as: (Monoid f, Monoid g) => Monoid (f, g)
prod (v1, x1) (v2, x2) = (v1 &&& v2 , (x1, x2))

asyncProd :: Monoid f => Monoid g => Automata s f a -> Automata s g b -> Automata (Either s s) (f, g) (a, b)
-- well-defined by product monoid typeclass
asyncProd (v1, x1) (v2, x2) = (go, (x1, x2))
    where
        go (Left a) = (v1 a, mempty)
        go (Right a) = (mempty, v2 a)

-- Section 5: relating language and MFAs

-- (a -> Bool) -> m a -> Bool

--k :: Functor m => (a -> Bool) -> m a -> Bool
--k l xm = l . _ $ xm

-- Type equation: Functor m => (a -> Bool) -> X -> (m a -> Bool). What solutions are there in X? What if we change Functor to Monad?

a :: (a -> Bool) -> (m a -> a) -> m a -> Bool
a l alg = l . alg

b :: Functor m => (a -> Bool) -> (m Bool -> Bool) -> m a -> Bool
b l alg = alg . fmap l

liftLabel :: Functor f => (f Bool -> Bool) -> Label a  -> Label (f a)
liftLabel alg accept = Label $ alg . fmap (satisfies accept)

class Monad f => Speakable f where -- can monad be weakened?
    alg :: f Bool -> Bool
    speak :: Label a -> Label (f a)
    speak = liftLabel alg
    lang' :: MFA s f a -> Label a -> Label [s]
    lang' aut = lang aut . speak

instance Speakable [] where
    alg = or -- alternative choice of and or xor are interesting

instance Speakable Maybe where
    alg = isJust

instance Monoid a => Speakable ((,) a) where -- generalisation of printing monad --printing monad does work
    alg = snd

{-instance (Speakable f, Speakable g) => Speakable (Product f g) where
    alg (Pair xf yf) = alg xf || alg yf

instance (Speakable f, Speakable g) => Speakable (Sum f g) where
    alg (InL xf) = alg xf
    alg (InR yf) = alg yf-}

--instance Speakable Prob -- : interesting cases with distribution of accepts and taking the mode for algebra

--instance Speakable CPS -- using the forall version of CPS, using id as continuation

--memory doesn't work

--parser kind of works, surprisingly. Check by parsing the empty string and see if it succeeds

-- liftLabelND :: Label a -> Label [a]
-- liftLabelND = liftLabel or
--
-- liftLabelMaybe :: Label a -> Label (Maybe a)
-- liftLabelMaybe = liftLabel isJust
{-
N1: for Speakable m, aut :: MFA s m a, forall accept :: Label a
    lang (determinise aut) (speak accept) = lang' aut accept
N2: for Speakable f, aut :: DFA s a, forall accept :: Label a
    lang' (lift aut) accept = lang aut accept

Proof of N1:
    LHS = lang (determinise aut) (speak accept)
        = Label (satisfies (speak accept) . run (determinise aut))
        = Label (satisfies (liftLabel alg accept) . run (determinise aut))
        = Label (alg . fmap (satisfies accept) . run (determinise aut))

        determinise aut
            = ((\v a xm -> v a =<< xm) *** id) (v, x) where aut = (v, x)
            = ((\a xm -> v a =<< xm), x)

        run (determinise aut) w
            = act (induce (\a xm -> v a =<< xm) w) x
            = induce (\a xm -> v a =<< xm) w x
            = foldr (\y ys -> (\a xm -> v a =<< xm) y <> ys) mempty w x
            = foldr (\y ys -> (\xm -> v y =<< xm) <> ys) mempty w x
            = foldr (\y ys -> (v y =<<) <> ys) mempty w x
            = foldr (\y ys -> (v y =<<) . ys) mempty w x

        run (v, x) w
            = act (induce v w) x
            = induce v w x
            = foldr (\y ys -> v y <> ys) w x
            = foldr (\y ys -> v y >=> ys) w x

        Now
        (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
        f >=> g = \x -> f x >>= \y -> g y
                = \x -> f x >>= g
        So
        v y >=> ys = \x -> v y x >>= ys

        (v y =<<) . ys = \x -> ((v y =<<) . ys) x
            = \x -> (v y =<< ys x)

    RHS = lang' aut accept
        = lang aut (speak accept)

-}


{-
induce :: Monoid f => (s -> f) -> ([s] -> f)
induce v = foldr (\x xs -> v x <> xs) mempty

Induce induces a homomorphism:
    induce v [] = mempty :: f
    induce v (a ++ b) = induce v a <> induce v b
Proof:
    induce v [] = foldr (\x xs -> v x <> xs) mempty [] = mempty
    induce v (a ++ b)
        = foldr c mempty (a ++ b)
        (where c = \x xs -> v x <> xs)
        = foldr c mempty (foldr (:) b a)
        [++ b = foldr (:) b]
        = foldr c (foldr c mempty b) a
        [
        Fold fusion.
        foldr c mempty ((:) x y) = c x (foldr c mempty y)
            = (\s t -> c s t) x (foldr c mempty y)
        ]
        = foldr c (induce v b) a

Composition lemma:
    If g is a monoid, and if w :: f -> g is a monoid homomorphism, then
    w . induce v = induce (w . v)
Proof:
    STATEMENT OF FOLD FUSION:
        https://flolac.iis.sinica.edu.tw/2020/FP-FoldHandouts.pdf
        If p :: a -> b -> b, e :: b, r :: b -> c, q :: a -> c -> c, then
            r . foldr p e = foldr q (r e)
        if r (p x y) = q x (r y) for all x and y

    w ((\x xs -> v x <> xs) x xs)
        = w (v x <> xs)
        = w (v x) <> w xs [w hom]
        = (\x y -> (w . v) x <> y) x (w xs)
    Hence by fold fusion,
    w . induce v = w . foldr (\x xs -> v x <> xs) mempty [defn induce]
                 = foldr (\x y -> (w . v) x <> y) (w mempty) [fusion]
                 = foldr (\x y -> (w . v) x <> y) mempty [w hom]
                 = induce (w . v) [defn induce]

Corollary:
    act . induce v = induce (act . v)
-}
