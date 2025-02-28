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
    {-# INLINE monoToList #-}
    monoToList :: MonoCompose f a -> [ElementOf a]
    monoToList (MonoCompose xs) = concat $ toList (fmap monoToList xs)

    {-# INLINE monoFoldMap #-}
    monoFoldMap :: Monoid m => (ElementOf a -> m) -> MonoCompose f a -> m
    monoFoldMap f (MonoCompose xs) = foldl' (<>) mempty $ fmap (monoFoldMap f) xs

    {-# INLINE monoNull #-}
    monoNull :: MonoCompose f a -> Bool
    monoNull (MonoCompose xs) = foldl' (&&) True $ fmap monoNull xs

    {-# INLINE monoLength #-}
    monoLength :: MonoCompose f a -> Word
    monoLength (MonoCompose xs) = foldl' (+) 0 $ fmap monoLength xs
