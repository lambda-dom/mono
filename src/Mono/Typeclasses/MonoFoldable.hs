{- |
Module: Mono.Typeclasses.MonoFoldable

The @MonoFoldable@ typeclass.
-}

module Mono.Typeclasses.MonoFoldable (
    -- * Typeclasses.
    MonoFoldable (..),
) where

-- Imports.
-- Base.
import Data.Foldable (Foldable (..))
import Data.List (singleton)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (isNothing)

-- Package.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))


{- | The typeclass for monofunctors that can be folded over, the monomorphic version of 'Foldable'.

All methods have a default implementation in terms of 'monotoList' and it is implicitly assumed
that any overriding definitions are extensionally equal to the default ones. Given this assumption:

__Mononaturality__: @'monotoList' :: s -> [a]@ is mononatural.
-}
class MonoFunctor a s => MonoFoldable a s where
    {-# MINIMAL monotoList #-}

    {- | Convert a monofoldable to a list. -}
    monotoList :: s -> [a]

    {- | Strict fold with a function @'Monoid' m => a -> m@. -}
    {-# INLINEABLE monofoldMap #-}
    monofoldMap :: Monoid m => (a -> m) -> s -> m
    monofoldMap f = foldl' (<>) mempty . fmap f . monotoList

    {- | Lazy right-associative fold of a monofoldable. -}
    {-# INLINEABLE monofoldr #-}
    monofoldr :: (a -> b -> b) -> b -> s -> b
    monofoldr f x = foldr f x . monotoList

    {- | Right-associative, strict in the accumulator, fold of a monofoldable. -}
    {-# INLINEABLE monofoldl #-}
    monofoldl :: (b -> a -> b) -> b -> s -> b
    monofoldl f x xs = foldl' f x $ monotoList xs

    {- | Return true if the monofoldable has no elements. -}
    {-# INLINEABLE mononull #-}
    mononull :: s -> Bool
    mononull = null . monotoList

    {- | The number of elements in the monofoldable. -}
    {-# INLINEABLE monolength #-}
    monolength :: s -> Word
    monolength = fromIntegral . length . monotoList

    {- | Return True if @elem@ is an element of the monofoldable. -}
    {-# INLINEABLE monoelem #-}
    monoelem :: Eq a => a -> s -> Bool
    monoelem x = (x `elem`) . monotoList


-- Foldable instances.
instance MonoFoldable a [a] where
    {-# INLINE monotoList #-}
    monotoList :: [a] -> [a]
    monotoList = id

    {-# INLINEABLE monofoldMap #-}
    monofoldMap :: Monoid m => (a -> m) -> [a] -> m
    monofoldMap f = foldl' (<>) mempty . fmap f

    {-# INLINE monofoldr #-}
    monofoldr :: (a -> b -> b) -> b -> [a] -> b
    monofoldr = foldr

    {-# INLINE monofoldl #-}
    monofoldl :: (b -> a -> b) -> b -> [a] -> b
    monofoldl = foldl'

    {-# INLINE mononull #-}
    mononull :: [a] -> Bool
    mononull = null

    {-# INLINE monolength #-}
    monolength :: [a] -> Word
    monolength = fromIntegral . length

    {-# INLINE monoelem #-}
    monoelem :: Eq a => a -> [a] -> Bool
    monoelem = elem

instance MonoFoldable a (NonEmpty a) where
    {-# INLINE monotoList #-}
    monotoList :: NonEmpty a -> [a]
    monotoList = toList

    {-# INLINE monofoldr #-}
    monofoldr :: (a -> b -> b) -> b -> NonEmpty a -> b
    monofoldr = foldr

    {-# INLINE monofoldl #-}
    monofoldl :: (b -> a -> b) -> b -> NonEmpty a -> b
    monofoldl = foldl'

    {-# INLINE mononull #-}
    mononull :: NonEmpty a -> Bool
    mononull = null

    {-# INLINE monolength #-}
    monolength :: NonEmpty a -> Word
    monolength = fromIntegral . length

    {-# INLINE monoelem #-}
    monoelem :: Eq a => a -> NonEmpty a -> Bool
    monoelem = elem

instance MonoFoldable a (Maybe a) where
    {-# INLINE monotoList #-}
    monotoList :: Maybe a -> [a]
    monotoList = maybe [] singleton

    {-# INLINEABLE monofoldMap #-}
    monofoldMap :: Monoid m => (a -> m) -> Maybe a -> m
    monofoldMap = maybe mempty

    {-# INLINE monofoldr #-}
    monofoldr :: (a -> b -> b) -> b -> Maybe a -> b
    monofoldr = foldr

    {-# INLINE monofoldl #-}
    monofoldl :: (b -> a -> b) -> b -> Maybe a -> b
    monofoldl = foldl'

    {-# INLINE mononull #-}
    mononull :: Maybe a -> Bool
    mononull = isNothing

    {-# INLINE monolength #-}
    monolength :: Maybe a -> Word
    monolength = maybe 0 (const 1)

    {-# INLINE monoelem #-}
    monoelem :: Eq a => a -> Maybe a -> Bool
    monoelem x = (Just x ==)

instance MonoFoldable a (Either e a) where
    {-# INLINE monotoList #-}
    monotoList :: Either e a -> [a]
    monotoList = either (const []) singleton

    {-# INLINEABLE monofoldMap #-}
    monofoldMap :: Monoid m => (a -> m) -> Either e a -> m
    monofoldMap = either (const mempty)

    {-# INLINE monofoldr #-}
    monofoldr :: (a -> b -> b) -> b -> Either e a -> b
    monofoldr = foldr

    {-# INLINE monofoldl #-}
    monofoldl :: (b -> a -> b) -> b -> Either e a -> b
    monofoldl = foldl'

    {-# INLINE mononull #-}
    mononull :: Either e a -> Bool
    mononull = either (const True) (const False)

    {-# INLINE monolength #-}
    monolength :: Either e a -> Word
    monolength = either (const 0) (const 1)

    {-# INLINE monoelem #-}
    monoelem :: Eq a => a -> Either e a -> Bool
    monoelem x = either (const False) (x ==)
