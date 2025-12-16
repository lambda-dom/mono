{- |
Module: Mono.Typeclasses.MonoFunctor

The @MonoFunctor@ typeclass.
-}

module Mono.Typeclasses.MonoFunctor (
    -- * Typeclasses.
    MonoFunctor (..),
) where

-- Imports.
-- Base.
import Data.List.NonEmpty (NonEmpty)


{- | The typeclass for pure monofunctors, the monomorphic version of 'Functor'. -}
class MonoFunctor a s | s -> a where
    {-# MINIMAL monomap #-}

    {- | Map over a monofunctor. -}
    monomap :: (a -> a) -> s -> s


-- Functor instances.
instance MonoFunctor a [a] where
    {-# INLINE monomap #-}
    monomap :: (a -> a) -> [a] -> [a]
    monomap = fmap

instance MonoFunctor a (NonEmpty a) where
    {-# INLINE monomap #-}
    monomap :: (a -> a) -> NonEmpty a -> NonEmpty a
    monomap = fmap

instance MonoFunctor a (Maybe a) where
    {-# INLINE monomap #-}
    monomap :: (a -> a) -> Maybe a -> Maybe a
    monomap = fmap

instance MonoFunctor a (Either e a) where
    {-# INLINE monomap #-}
    monomap :: (a -> a) -> Either e a -> Either e a
    monomap = fmap
