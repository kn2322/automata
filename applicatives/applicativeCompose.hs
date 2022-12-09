-- proving that applicative composition is associative
{-
Next:
    * Write an applicative normal form calculator based on Constrained Applicative Problem
        * extend the normal form calculator with the lambda calculus for comparing terms at the head of the computation
    * Prove that applicative normal form exists
        * induction
    * Prove that equivalence of applicative normal forms (head same, atoms applied in same order) <-> for all applicatives, the expressions are equal
        * By induction on application of laws?


TODO:
    * See constrained monad problem http://neilsculthorpe.com/publications/constrained-monad-problem.pdf, free applicative functors, https://www.paolocapriotti.com/assets/applicative.pdf, and McBride/Paterson for implementation of free applicative
        * Understand why the applicative normal form exists (by the algorithm in constrained monad problem)
    * Perform applicative normal forms of LHS and RHS, using point-wise calculation to find the beta normal form of both pure functions at the head of the applicative normal form.
        * done. <.> is associative
        * Is pure flip <*> f <*> y <*> x = f <*> x <*> y?
        * Is pure ($) <*> f <*> x = f <*> x?
        * If two applicative expressions in normal form are not equivalent, does this mean there is an applicative instance where they have different interpretations? (completeness/soundness?)
        * Can applicatives be characterised as a monoid (<.>) with an action (<*>)? Monads, Functors?
        * Is there a way of making the derivation for RHS painless? The laws seem to be easy for LHS but hard for RHS. (maybe a higher algebra of applicatives)
        * Can we check the proof/automate the proof by implementing the lambda calculus with applicatives laws?
        * Is there an algebra of compositions of compositions, as appears in applicative normal form calculations?

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

{-data Free a where
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
-}
