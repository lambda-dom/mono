{- |
Module: Data.Types.MonoCompose

The @MonoCompose@ newtype-wrapper type.
-}

module Data.Types.MonoCompose (
    -- * Types.
    MonoCompose (..),
) where

-- Imports.
-- Base.
import Data.Foldable (Foldable (..))

-- Package.
import Data.MonoFunctor (MonoFunctor (..))
import Data.MonoFoldable (MonoFoldable (..))


{- | Newtype-wrapper around @f a@ for a functor @f@ and a monofunctor @a@. -}
newtype MonoCompose f a = MonoCompose (f a)
    deriving newtype (Eq, Show)


-- Instances.
instance (Functor f, MonoFunctor a) => MonoFunctor (MonoCompose f a) where
    type ElementOf (MonoCompose f a) = ElementOf a

    monomap :: (ElementOf a -> ElementOf a) -> MonoCompose f a -> MonoCompose f a
    monomap f (MonoCompose xs) = MonoCompose (fmap (monomap f) xs)

instance (Functor f, Foldable f, MonoFoldable a) => MonoFoldable (MonoCompose f a) where
    {-# INLINE monotoList #-}
    monotoList :: MonoCompose f a -> [ElementOf a]
    monotoList (MonoCompose xs) = concat $ toList (fmap monotoList xs)

    {-# INLINE monofoldMap #-}
    monofoldMap :: Monoid m => (ElementOf a -> m) -> MonoCompose f a -> m
    monofoldMap f (MonoCompose xs) = foldl' (<>) mempty $ fmap (monofoldMap f) xs

    {-# INLINE mononull #-}
    mononull :: MonoCompose f a -> Bool
    mononull (MonoCompose xs) = foldl' (&&) True $ fmap mononull xs

    {-# INLINE monolength #-}
    monolength :: MonoCompose f a -> Word
    monolength (MonoCompose xs) = foldl' (+) 0 $ fmap monolength xs
