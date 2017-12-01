{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module Cheatsheet where

import Prelude hiding (mapM)
import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random

import qualified Data.Vector as V


{--

Monoid (n)
Functor (r)
Applicative (p)
Monad (m)
Traversable (t)
Alternative (v)
Foldable (d)

Functor --> Applicative --> Monad --> MonadPlus
      |              \
      |               \
      |                --> Alternative
      |
       --> Traversable
       |
       |
Foldable


fmap :: (a -> b) -> r a -> r b

pure :: a -> p a
(<*>) :: p (a -> b) -> p a -> p b
(*>) :: p a -> p b -> p b
(*>) = \pa pb -> pure (const id) <*> pa <*> pb
(<$>) = liftA = <*> . pure = fmap

return :: a -> m a
(>>=) :: m a -> (a -> m b) -> m b
(>>) :: m a -> m b -> m b
(>>) = \ma mb -> ma >>= \_ -> mb
(>>) = \ma mb -> return (const id) `ap` ma `ap` mb
(>>) = (*>)
return = pure
ap = <*>

join :: m (m a) -> m a
join = \mma -> mma >>= id

liftM :: (a -> b) -> m a -> m b
liftM = liftA = <$> = (<*>) . pure = ap . return = fmap

sequence :: t (m a) -> m (t a)
sequence_ :: d (m a) -> m ()
sequenceA :: t (p a) -> p (t a)
sequenceA_ :: d (p a) -> p ()

mapM :: (a -> m b) -> t a -> m (t b)
mapM_ :: (a -> m b) -> d a -> m ()
traverse :: (a -> p b) -> t a -> p (t b)
traverse_ :: (a -> p b) -> d a -> p ()

sequence_ x = sequence x >> return ()
sequence_ = (>> return ()) . sequence

sequence_ = foldr (>>) (return ())
sequenceA_ = foldr (*>) (pure ())

mapM_ f = foldr ((>>) . f) (return ())
traverse_ f = foldr ((*>) . f) (pure ())

mapM_ = (sequence_ .) . fmap
sequence_ = mapM_ id
traverse_ = (sequenceA_ .) . fmap
sequenceA_ = traverse_ id

mapM = (sequence .) . fmap
sequence = mapM id
traverse = (sequenceA .) . fmap
sequenceA = traverse id

foldMap :: (a -> n) -> d a -> n
foldMap :: (a -> n) -> t a -> n -- If the Foldable (d) is also a Traversable (t).
foldMap f = getConst . traverse (Const . f)
foldMap f = fst . traverse ((\x -> (x,x)) . f) -- If the Foldable (d) is also a (t) ie Taversable.
foldMap f = . sequenceA . fmap ((\x -> (x,x) . f)
--}

