{-# LANGUAGE CPP                        #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

{-|
This library provides strict versions of many
functions in base, as well as a few functions
that do not have lazy versions that exist in
base (see the section on Folds).

Many functions in this library have an increased
constraint from Functor/Applicative to Monad in
order to achieve strictness in their arguments
and/or result.
-}

module Constrictor
  ( 
    -- * Strict 'lift-like' functions 
    (<$!>)
  , fmap'
  , liftM'
  , liftM2'
  , liftM3'
  , liftM4'
  , liftM5'
  , ap' 
  , traverse'
  , traverse''
  , mapM'
    
    -- * Folds
    -- ** Lazy monoidal folds
  , foldrMap
  , foldlMap
    -- ** Strict monoidal folds
  , foldrMap'
  , foldlMap'
    -- ** Lazy applicative folds
  , foldlMapA
  , foldrMapA
   -- ** Strict monadic folds
  , foldlMapM'
  , foldrMapM'
    -- * Types
    -- ** Wrapped applicative functor
  , Ap(..)
  ) where

import Prelude hiding (foldr,foldl)

import Control.Applicative (Alternative, Applicative(..), liftA2)
import Control.Monad (MonadPlus, ap, liftM, liftM2)
#if MIN_VERSION_base(4,9,0)
import Control.Monad.Fail (MonadFail)
#endif
import Control.Monad.Fix  (MonadFix)
import Control.Monad.Trans.Cont (ContT(..), cont)
import Data.Foldable
import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity (runIdentity)
import Data.Monoid hiding ((<>))
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup
#endif
import Data.Traversable (traverse,Traversable)
import GHC.Generics (Generic,Generic1)

-- | A wrapped applicative functor.
--   Please note that base 4.12.0.0 will include this type,
--   and it will be removed from this library at that point.
newtype Ap f a = Ap { getAp :: f a }
  deriving ( Alternative, Applicative
           , Enum, Eq, Foldable, Functor
           , Generic
#if MIN_VERSION_base(4,6,0)
           , Generic1
#endif
           , Monad
#if MIN_VERSION_base(4,9,0)
           , MonadFail
#endif
           , MonadFix, MonadPlus
           , Num, Ord, Read, Show, Traversable
           )

#if MIN_VERSION_base(4,9,0)
instance (Applicative f, Semigroup a) => Semigroup (Ap f a) where
  (Ap x) <> (Ap y) = Ap $ liftA2 (<>) x y
#endif

instance (Applicative f, Monoid a) => Monoid (Ap f a) where
  mempty = Ap $ pure mempty
#if !(MIN_VERSION_base(4,11,0))
  mappend (Ap x) (Ap y) = Ap $ liftA2 (mappend) x y
#endif

-- | Lazy in the monoidal accumulator. Monoidal accumulation
--   happens from left to right.
foldlMapA :: forall t b a f. (Foldable t, Monoid b, Applicative f) => (a -> f b) -> t a -> f b
foldlMapA f = foldr (\x y -> liftA2 mappend (f x) y) (pure mempty)

-- | Lazy in the monoidal accumulator. Monoidal accumulation
--   happens from left to right.
foldrMapA :: forall t b a f. (Foldable t, Monoid b, Applicative f) => (a -> f b) -> t a -> f b
foldrMapA f = foldl (\y x -> liftA2 (flip mappend) (f x) y) (pure mempty)

-- | Strict in the monoidal accumulator.
--   For monads strict in the left argument of bind,
--   this will run in constant space.
--   Monoidal accumulation happens from left to right.
foldlMapM' :: forall t b a m. (Foldable t, Monoid b, Monad m) => (a -> m b) -> t a -> m b
foldlMapM' f xs = foldr f' return xs mempty
  where
  f' :: a -> (b -> m b) -> b -> m b
  f' x k bl = do
    !br <- f x
    k $! (mappend bl br) 

-- Strict in the monoidal accumulator. 
-- Monoidal accumulation happens from left to right.
foldrMapM' :: forall t b a m. (Foldable t, Monoid b, Monad m) => (a -> m b) -> t a -> m b
foldrMapM' f xs = foldl f' return xs mempty
  where
  f' :: (b -> m b) -> a -> b -> m b
  f' k x br = do
    !bl <- f x
    k $! (mappend bl br) 

infixl 4 <$!>, `fmap'`, `liftM'`

-- | This is 'Data.Functor.<$>', but strict in its
-- argument and result.
--
-- This is re-defined in this module, and not
-- just re-exported from @'Control.Monad'@.
-- The reason for this is that there is no way
-- to hide the docs for re-exports with Haddocks.
--
-- In the common case that one might import
-- @'Control.Monad'@, we recommend structuring
-- imports like so:
--
-- @
-- import Control.Monad hiding ((<$!>))
-- import Constrictor
-- @
--
-- or
--
-- @
-- import Control.Monad
-- import Constrictor hiding ((<$!>))
-- @
--
-- There should be no side effects (i.e.
-- naming/scoping conflicts) introduced as a
-- result of structuring one's imports in this way.
(<$!>) :: Monad m => (a -> b) -> m a -> m b
{-# INLINE (<$!>) #-}
f <$!> m = do
  !x <- m
  return $! f x

-- | This is 'Data.Functor.fmap', but strict in its
-- argument and result.
--
-- Note this is equivalent to '<$!>',
-- and is provided for convenience.
fmap' :: Monad m => (a -> b) -> m a -> m b
{-# INLINE fmap' #-}
fmap' = (<$!>)

-- | This is 'Control.Monad.liftM', but strict in its
-- argument and result.
--
-- Note this is equivalent to '<$!>',
-- and is provided for convenience.
liftM' :: Monad m => (a -> b) -> m a -> m b
{-# INLINE liftM' #-} 
liftM' = (<$!>)

-- | This is 'Control.Monad.liftM2', but strict in its
-- arguments and result.
liftM2' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
{-# INLINE liftM2' #-}
liftM2' f a b = do
  !x <- a
  !y <- b
  return $! f x y

-- | This is 'Control.Monad.liftM3', but strict in its
-- arguments and result.
liftM3' :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
{-# INLINE liftM3' #-}
liftM3' f a b c = do
  !x <- a
  !y <- b
  !z <- c
  return $! f x y z

-- | This is 'Control.Monad.liftM4', but strict in its
-- arguments and result.
liftM4' :: Monad m => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e 
{-# INLINE liftM4' #-}
liftM4' f a b c d = do
  !x <- a
  !y <- b
  !z <- c
  !u <- d
  return $! f x y z u

-- | This is 'Control.Monad.liftM5', but strict in its
-- arguments and result.
liftM5' :: Monad m => (a -> b -> c -> d -> e -> f) -> m a -> m b -> m c -> m d -> m e -> m f
{-# INLINE liftM5' #-}
liftM5' f a b c d e = do
  !x <- a
  !y <- b
  !z <- c
  !u <- d
  !v <- e
  return $! f x y z u v

-- | This is 'Control.Monad.ap', but strict in its
-- arguments and result.
ap' :: Monad m => m (a -> b) -> m a -> m b
{-# INLINE ap' #-}
ap' m1 m2 = do
  !f <- m1
  !x <- m2
  return $! f x

-- | Strict version of 'Data.Traversable.traverse'.
traverse' :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
{-# INLINE traverse' #-}
traverse' f = fmap (runIdentity . evalContT) . getCompose . traverse (Compose . fmap (\a -> cont $ \k -> k $! a) . f)

-- | Stricter version of 'Data.Traversable.traverse'.
traverse'' :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
{-# INLINE traverse'' #-}
#if MIN_VERSION_base(4,8,0)
traverse'' f = fmap' (runIdentity . evalContT) . getCompose . traverse (Compose . fmap' (\a -> cont $ \k -> k $! a) . f)
#else

newtype WrappedMonad m a = WrappedMonad { unwrapMonad :: m a }
  deriving (Monad)

instance Monad m => Functor (WrappedMonad m) where
    fmap f (WrappedMonad v) = WrappedMonad (liftM f v)

instance Monad m => Applicative (WrappedMonad m) where
    pure = WrappedMonad . pure
    WrappedMonad f <*> WrappedMonad v = WrappedMonad (f `ap` v)
    liftA2 f (WrappedMonad x) (WrappedMonad y) = WrappedMonad (liftM2 f x y)

traverse'' f = unwrapMonad . fmap' (runIdentity . evalContT) . getCompose . traverse (Compose . fmap' (\a -> cont $ \k -> k $! a) . (\x -> WrappedMonad (f x)))
#endif

-- this is copied from transformers for backwards compatibility
evalContT :: (Monad m) => ContT r m r -> m r
evalContT m = runContT m return
{-# INLINE evalContT #-}

-- | Strict version of 'Control.Monad.mapM'.
--
-- This is just 'traverse'' specialised to 'Monad'.
mapM' :: (Traversable t, Monad m) => (a -> m b) -> t a-> m (t b)
{-# INLINE mapM' #-}
#if MIN_VERSION_base(4,8,0)
mapM' = traverse'
#else
mapM' f xs = unwrapMonad (traverse' (\x -> WrapMonad (f x)) xs)
#endif

-- The INLINES used below allow more list functions to fuse.
-- See Trac #9848.
{-# INLINE foldrMap  #-}
{-# INLINE foldrMap' #-}
{-# INLINE foldlMap  #-}
{-# INLINE foldlMap' #-}

-- | Map each element of a foldable structure to a monoid,
-- and combine the results. This function is left-associative.
--
-- The operator is applied lazily. For a strict version, see
-- 'foldlMap''.
foldlMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
foldlMap f = foldl (flip (mappend . f)) mempty

-- | Map each element of a foldable structure to a monoid,
-- and combine the results. This function is right-associative.
--
-- Note that this is equivalent to 'Data.Foldable.foldMap'.
foldrMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
foldrMap f = foldr (mappend . f) mempty

-- | Map each element of a foldable structure to a monoid,
-- and combine the results. This function is left-associative.
--
-- The operator is applied strictly. For a lazy version, see
-- 'foldlMap'.
foldlMap' :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
foldlMap' f = foldl' (flip (mappend . f)) mempty 

-- | Map each element of a foldable structure to a monoid,
-- and combine the results. This function is right-associative.
--
-- Note that this is equivalent to 'Data.Foldable.foldMap',
-- but is strict.
foldrMap' :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
foldrMap' f = foldr' (mappend . f) mempty
