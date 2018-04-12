{-# LANGUAGE CPP                        #-}
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
-}

module Control.Monad.Constrictor
  ( 
    -- * Strict monadic functions 
    (<$!>)
  , fmap'
  , liftM'
  , liftM2'
  , liftM3'
  , liftM4'
  , liftM5'
  , ap' 
    
    -- * Strict traversable functions
  , traverse'
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

import Control.Applicative
import Control.Monad (MonadPlus)
import Control.Monad.Fail (MonadFail)
import Control.Monad.Fix  (MonadFix)
import Control.Monad.Trans.Cont (evalCont, cont)
import Data.Foldable
import Data.Functor.Compose (Compose(..))
import Data.Monoid hiding ((<>))
import Data.Semigroup
import Data.Traversable (traverse)
import GHC.Generics (Generic,Generic1)

-- | A wrapped applicative functor.
--   Please note that base 4.12.0.0 will include this type,
--   and it will be removed at that point.
newtype Ap f a = Ap { getAp :: f a }
  deriving ( Alternative, Applicative
           , Enum, Eq, Foldable, Functor
           , Generic, Generic1
           , Monad, MonadFail, MonadFix, MonadPlus
           , Num, Ord, Read, Show, Traversable
           )

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
foldrMapM' f xs = foldl f' pure xs mempty
  where
  f' :: (b -> m b) -> a -> b -> m b
  f' k x br = do
    bl <- f x
    k $! (mappend bl br) 

infixl 4 <$!>, `fmap'`, `liftM4'`

-- | Strict version of 'Data.Functor.<$>'
(<$!>) :: Monad m => (a -> b) -> m a -> m b
{-# INLINE (<$!>) #-}
f <$!> m = do
  x <- m
  pure $! f x

-- Note this is equivalent to '<$!>',
-- and is provided for convenience.
fmap' :: Monad m => (a -> b) -> m a -> m b
{-# INLINE fmap' #-}
fmap' = (<$!>)

-- | Strict version of 'Control.Monad.liftM'.
--
-- Note this is equivalent to '<$!>',
-- and is provided for convenience.
liftM' :: Monad m => (a -> b) -> m a -> m b
{-# INLINE liftM' #-} 
liftM' = (<$!>)

-- | Strict version of 'Control.Monad.liftM2'.
liftM2' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
{-# INLINE liftM2' #-}
liftM2' f a b = do
  x <- a
  y <- b
  pure $! f x y

-- | Strict version of 'Control.Monad.liftM3'.
liftM3' :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
{-# INLINE liftM3' #-}
liftM3' f a b c = do
  x <- a
  y <- b
  z <- c
  pure $! f x y z

-- | Strict version of 'Control.Monad.liftM4'.
liftM4' :: Monad m => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e 
{-# INLINE liftM4' #-}
liftM4' f a b c d = do
  x <- a
  y <- b
  z <- c
  u <- d
  pure $! f x y z u

-- | Strict version of 'Control.Monad.liftM5'.
liftM5' :: Monad m => (a -> b -> c -> d -> e -> f) -> m a -> m b -> m c -> m d -> m e -> m f
{-# INLINE liftM5' #-}
liftM5' f a b c d e = do
  x <- a
  y <- b
  z <- c
  u <- d
  v <- e
  pure $! f x y z u v

-- | Strict version of 'Control.Monad.ap'
ap' :: Monad m => m (a -> b) -> m a -> m b
{-# INLINE ap' #-}
ap' m1 m2 = do
  f <- m1
  x <- m2
  pure $! f x

-- | Strict version of 'Data.Traversable.traverse'.
traverse' :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
{-# INLINE traverse' #-}
traverse' f = fmap evalCont . getCompose . traverse (Compose . fmap (\a -> cont $ \k -> k $! a) . f)

-- | Strict version of 'Control.Monad.mapM'.
--
-- This is just 'traverse'' specialised to 'Monad'.
mapM' :: (Traversable t, Monad m) => (a -> m b) -> t a-> m (t b)
{-# INLINE mapM' #-}
mapM' = traverse'

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
