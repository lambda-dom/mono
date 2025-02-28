{- |
Module: Data.MonoFoldable

The @MonoFoldable@ typeclass.
-}

module Data.MonoFoldable (
    -- * Typeclasses.
    MonoFoldable (..),
) where

-- Imports.
-- Base.
import Data.Foldable (Foldable (..))
import Data.List (singleton)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (isNothing)
import Data.Word (Word8)

-- Libraries.
import qualified Data.ByteString as Bytes (ByteString, unpack, null, length)
import qualified Data.ByteString.Lazy as LazyBytes (ByteString, unpack, null, length)
import qualified Data.Text as Text (Text, unpack, null, length)
import qualified Data.Text.Lazy as LazyText (Text, unpack, null, length)
import Data.Sequence (Seq)
import Data.Vector (Vector)

-- Package.
import Data.MonoFunctor (MonoFunctor (..))


{- | The typeclass for monofunctors that can be folded over, the monomorphic version of 'Foldable'. -}
class MonoFunctor f => MonoFoldable f where
    {-# MINIMAL monoToList #-}

    {- | Convert a monofoldable to a list. -}
    monoToList :: f -> [ElementOf f]

    {- | Strict fold with a function @'Monoid' m => 'ElementOf' f -> m@. -}
    {-# INLINE monoFoldMap #-}
    monoFoldMap :: Monoid m => (ElementOf f -> m) -> f -> m
    monoFoldMap f = foldl' (<>) mempty . fmap f . monoToList

    {- | Lazy right-associative fold of a monomorphic container. -}
    {-# INLINE monoFoldr #-}
    monoFoldr :: (ElementOf f -> a -> a) -> a -> f -> a
    monoFoldr f x = foldr f x . monoToList

    {- | Right-associative, strict in the accumulator, fold of a monomorphic container. -}
    {-# INLINE monoFoldl #-}
    monoFoldl :: (a -> ElementOf f -> a) -> a -> f -> a
    monoFoldl f x = foldl' f x . monoToList

    {- | Return true if the monofoldable has no elements. -}
    {-# INLINE monoNull #-}
    monoNull :: f -> Bool
    monoNull = null . monoToList

    {- | The number of elements in the monofoldable. -}
    {-# INLINE monoLength #-}
    monoLength :: f -> Word
    monoLength = fromIntegral . length . monoToList


-- Monofoldable instances.
instance MonoFoldable Bytes.ByteString where
    {-# INLINE monoToList #-}
    monoToList :: Bytes.ByteString -> [Word8]
    monoToList = Bytes.unpack

    {-# INLINE monoNull #-}
    monoNull :: Bytes.ByteString -> Bool
    monoNull = Bytes.null

    {-# INLINE monoLength #-}
    monoLength :: Bytes.ByteString -> Word
    monoLength = fromIntegral . Bytes.length

instance MonoFoldable LazyBytes.ByteString where
    {-# INLINE monoToList #-}
    monoToList :: LazyBytes.ByteString -> [Word8]
    monoToList = LazyBytes.unpack

    {-# INLINE monoNull #-}
    monoNull :: LazyBytes.ByteString -> Bool
    monoNull = LazyBytes.null

    {-# INLINE monoLength #-}
    monoLength :: LazyBytes.ByteString -> Word
    monoLength = fromIntegral . LazyBytes.length

instance MonoFoldable Text.Text where
    {-# INLINE monoToList #-}
    monoToList :: Text.Text -> [Char]
    monoToList = Text.unpack

    {-# INLINE monoNull #-}
    monoNull :: Text.Text -> Bool
    monoNull = Text.null

    {-# INLINE monoLength #-}
    monoLength :: Text.Text -> Word
    monoLength = fromIntegral . Text.length

instance MonoFoldable LazyText.Text where
    {-# INLINE monoToList #-}
    monoToList :: LazyText.Text -> [Char]
    monoToList = LazyText.unpack

    {-# INLINE monoNull #-}
    monoNull :: LazyText.Text -> Bool
    monoNull = LazyText.null

    {-# INLINE monoLength #-}
    monoLength :: LazyText.Text -> Word
    monoLength = fromIntegral . LazyText.length

instance MonoFoldable [a] where
    {-# INLINE monoToList #-}
    monoToList :: [a] -> [a]
    monoToList = id

    {-# INLINE monoFoldMap #-}
    monoFoldMap :: Monoid m => (a -> m) -> [a] -> m
    monoFoldMap f = foldl' (<>) mempty . fmap f

    {-# INLINE monoFoldr #-}
    monoFoldr :: (a -> b -> b) -> b -> [a] -> b
    monoFoldr = foldr

    {-# INLINE monoFoldl #-}
    monoFoldl :: (b -> a -> b) -> b -> [a] -> b
    monoFoldl = foldl'

    {-# INLINE monoNull #-}
    monoNull :: [a] -> Bool
    monoNull = null

    {-# INLINE monoLength #-}
    monoLength :: [a] -> Word
    monoLength = fromIntegral . length

instance MonoFoldable (NonEmpty a) where
    {-# INLINE monoToList #-}
    monoToList :: NonEmpty a -> [a]
    monoToList = toList

    {-# INLINE monoFoldMap #-}
    monoFoldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
    monoFoldMap f = foldl' (<>) mempty . fmap f

    {-# INLINE monoFoldr #-}
    monoFoldr :: (a -> b -> b) -> b -> NonEmpty a -> b
    monoFoldr = foldr

    {-# INLINE monoFoldl #-}
    monoFoldl :: (b -> a -> b) -> b -> NonEmpty a -> b
    monoFoldl = foldl'

    {-# INLINE monoNull #-}
    monoNull :: NonEmpty a -> Bool
    monoNull = null

    {-# INLINE monoLength #-}
    monoLength :: NonEmpty a -> Word
    monoLength = fromIntegral . length

instance MonoFoldable (Maybe a) where
    {-# INLINE monoToList #-}
    monoToList :: Maybe a -> [a]
    monoToList = maybe [] singleton

    {-# INLINE monoFoldMap #-}
    monoFoldMap :: Monoid m => (a -> m) -> Maybe a -> m
    monoFoldMap = maybe mempty

    {-# INLINE monoNull #-}
    monoNull :: Maybe a -> Bool
    monoNull = isNothing

    {-# INLINE monoLength #-}
    monoLength :: Maybe a -> Word
    monoLength = maybe 0 (const 1)

instance MonoFoldable (Either e a) where
    {-# INLINE monoToList #-}
    monoToList :: Either e a -> [a]
    monoToList = either (const []) singleton

    {-# INLINE monoFoldMap #-}
    monoFoldMap :: Monoid m => (a -> m) -> Either e a -> m
    monoFoldMap = either (const mempty)

    {-# INLINE monoNull #-}
    monoNull :: Either e a -> Bool
    monoNull = either (const True) (const False)

    {-# INLINE monoLength #-}
    monoLength :: Either e a -> Word
    monoLength = either (const 0) (const 1)

instance MonoFoldable (Seq a) where
    {-# INLINE monoToList #-}
    monoToList :: Seq a -> [a]
    monoToList = toList

    {-# INLINE monoFoldMap #-}
    monoFoldMap :: Monoid m => (a -> m) -> Seq a -> m
    monoFoldMap f = foldl' (<>) mempty . fmap f

    {-# INLINE monoFoldr #-}
    monoFoldr :: (a -> b -> b) -> b -> Seq a -> b
    monoFoldr = foldr

    {-# INLINE monoFoldl #-}
    monoFoldl :: (b -> a -> b) -> b -> Seq a -> b
    monoFoldl = foldl'

    {-# INLINE monoNull #-}
    monoNull :: Seq a -> Bool
    monoNull = null

    {-# INLINE monoLength #-}
    monoLength :: Seq a -> Word
    monoLength = fromIntegral . length

instance MonoFoldable (Vector a) where
    {-# INLINE monoToList #-}
    monoToList :: Vector a -> [a]
    monoToList = toList

    {-# INLINE monoFoldMap #-}
    monoFoldMap :: Monoid m => (a -> m) -> Vector a -> m
    monoFoldMap f = foldl' (<>) mempty . fmap f

    {-# INLINE monoFoldr #-}
    monoFoldr :: (a -> b -> b) -> b -> Vector a -> b
    monoFoldr = foldr

    {-# INLINE monoFoldl #-}
    monoFoldl :: (b -> a -> b) -> b -> Vector a -> b
    monoFoldl = foldl'

    {-# INLINE monoNull #-}
    monoNull :: Vector a -> Bool
    monoNull = null

    {-# INLINE monoLength #-}
    monoLength :: Vector a -> Word
    monoLength = fromIntegral . length
