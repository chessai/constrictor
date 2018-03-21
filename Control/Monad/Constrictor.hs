{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Control.Monad.Constrictor
  ( (<$!>)
  , fmap'
  , liftM'
  , liftM2'
  , traverse'
  , mapM'
 
  , Ap(..)
  
  -- * strict monadic folds
  , foldlMapM'
  , foldrMapM'
  
  -- * non-strict applicative folds for completeness 
  , foldlMapA
  , foldrMapA
  ) where

import Control.Applicative
import Control.Monad.Trans.Cont (evalCont, cont)
import Data.Foldable
import Data.Functor.Compose (Compose(..))
import Data.Monoid hiding ((<>))
import Data.Semigroup
import Data.Traversable (traverse)
import GHC.Generics (Generic)

infixl 4 <$!>

-- A wrapped Applicative Functor.
newtype Ap f a = Ap { getAp :: f a }
  deriving (Functor,Generic,Foldable,Traversable)

instance (Applicative f, Semigroup a) => Semigroup (Ap f a) where
  (Ap x) <> (Ap y) = Ap $ liftA2 (<>) x y

instance (Applicative f, Monoid a) => Monoid (Ap f a) where
  mempty = Ap $ pure mempty
#if !(MIN_VERSION_base(4,11,0))
  mappend (Ap x) (Ap y) = Ap $ liftA2 (mappend) x y
#endif

-- | Lazy in the monoidal accumulator. Monoidal accumulation
--   happens from left to right.
foldlMapA :: forall t b a f. (Foldable t, Monoid b, Applicative f) => (a -> f b) -> t a -> f b
foldlMapA f = foldr f' (pure mempty)
  where
    f' :: a -> f b -> f b
    f' x y = liftA2 mappend (f x) y

-- | Lazy in the monoidal accumulator. Monoidal accumulation
--   happens from left to right.
foldrMapA :: forall t b a f. (Foldable t, Monoid b, Applicative f) => (a -> f b) -> t a -> f b
foldrMapA f = foldl f' (pure mempty)
  where
    f' :: f b -> a -> f b
    f' y x = liftA2 (flip mappend) (f x) y

-- | Strict in the monoidal accumulator.
--   For monads strict in the left argument of bind,
--   this will run in constant space.
--   Monoidal accumulation happens from left to right.
foldlMapM' :: forall t b a m. (Foldable t, Monoid b, Monad m) => (a -> m b) -> t a -> m b
foldlMapM' f xs = foldr f' pure xs mempty
  where
  f' :: a -> (b -> m b) -> b -> m b
  f' x k bl = do
    br <- f x
    k $! (mappend bl br) 

-- Strict in the monoidal accumulator. 
-- Monoidal accumulation happens from left to right.
foldrMapM' :: forall t b a m. (Foldable t, Monoid b, Monad m) => (a -> m b) -> t a -> m b
foldrMapM' f xs = foldl f' return xs mempty
  where
  f' :: (b -> m b) -> a -> b -> m b
  f' k x br = do
    bl <- f x
    k $! (mappend bl br) 

(<$!>) :: Monad m => (a -> b) -> m a -> m b
f <$!> ma = do
  a <- ma
  pure $! f a

fmap' :: Monad m => (a -> b) -> m a -> m b
fmap' = (<$!>)

liftM' :: Monad m => (a -> b) -> m a -> m b
liftM' = (<$!>)

liftM2' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2' f a b = do
  x <- a
  y <- b
  pure $! f x y

traverse' :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
traverse' f = fmap evalCont . getCompose . traverse (Compose . fmap (\a -> cont $ \k -> k $! a) . f)

mapM' :: (Traversable t, Monad m) => (a -> m b) -> t a-> m (t b)
mapM' = traverse'
