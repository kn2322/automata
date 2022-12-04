-- Automata, types, and monads
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
--{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Arrow ((***))
import Control.Monad (join)

type Automata a = (a -> a, a)
type MonadicAutomata m a = (a -> m a, a)

--run :: (a -> a) -> a -> [a]
--run = iterate
run :: Automata a -> [a]
run = uncurry iterate

--runM :: Monad m => (a -> m a) -> a -> [m a]
--runM f x = iterate (>>= f) (return x)
runM :: Monad m => MonadicAutomata m a -> [m a]
runM = run . determinise -- the only way we know how to run a monadic automata is by determinising?

--determinise :: Monad m => ((a -> m a), a) -> ((m a -> m a), m a)
--determinise (f :: (a -> m a), x) = ((join .) . (fmap @m)) *** return $ (f, x)

--determinise :: Monad m => ((a -> m a), a) -> ((m a -> m a), m a)
--determinise :: Monad m => MonadicAutomata m a -> Automata (m a)
--determinise = ((join .) . fmap) *** return

determinise :: Monad m => MonadicAutomata m a -> Automata (m a)
determinise = (=<<) *** return

--lift :: Monad m => (a -> a, a) -> ((a -> m a), a)
lift :: Applicative m => Automata a -> MonadicAutomata m a -- premonad is okay
lift = (pure .) *** id

-- less than clear what this does
joinAut :: Monad m => MonadicAutomata m (m a) -> Automata (m a)
-- ((m a -> m (m a)), m a) -> (m a -> m a, m a)
joinAut = (join .) *** id

{-
laws:
joinAut . lift = id (comes from arrow laws and monad laws)
extract . lift = id (I'm honestly not sure if this is a good thing to have, since it's not total)
-}

class Applicative m => InjectiveMonad m where
    extract :: m a -> a
-- law: extract . pure = id
-- extract need not be total

instance InjectiveMonad Maybe where
    extract (Just x) = x
instance InjectiveMonad [] where
    extract = head

-- examples
powset :: MonadicAutomata [] a -> Automata [a]
powset = determinise @[]

nonBlock :: Applicative m => MonadicAutomata m a -> MonadicAutomata m (Maybe a)
nonBlock (f, x) = (modify f, Just x)
    where
        modify f Nothing = pure Nothing
        modify f (Just x) = fmap Just (f x)


type S = Char
type DFA a = MonadicAutomata ((->) S) a

type Subset = S -> Bool
type Lang = [S]

decide :: [S] -> (DFA a, (S -> Bool)) -> Bool
decide w = undefined

{-
-- why on earth doesn't this work?
type NFA a = MonadicAutomata ND a

newtype ND a = ND {runND :: S -> [a]}
--type family ND a where
    --ND a = S -> [a]
--type instance ND a = S -> [a]

instance Functor ND where
    fmap = ND . fmap . runND

instance Monad ND where
    return = ND . return . return
    xm >>= f = _  -- \s -> runND xm s >>= \x -> f x s
-}

{-
Partially applied type instances are naughty:
https://stackoverflow.com/questions/4922560/why-doesnt-typesynonyminstances-allow-partially-applied-type-synonyms-to-be-use

type DFA s a = MonadicAutomata ((->) s) a
type NFA s a = MonadicAutomata (ND s) a

type ND s a = s -> [a]
instance Monad (ND s) where
    return = return . return
    xm >>= f = \s -> xm s >>= \x -> f x s
-}

type Machine a = S -> (a, [S])
