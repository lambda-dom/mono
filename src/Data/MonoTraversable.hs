{- |
Module: Data.MonoTraversable

The @MonoTraversable@ typeclass.
-}

module Data.MonoTraversable (
    -- * Typeclasses.
    MonoTraversable (..),
) where

-- Imports.
-- Base.
import Data.Word (Word8)
import Data.List.NonEmpty (NonEmpty)
import Data.Sequence (Seq)

-- Libraries.
import qualified Data.ByteString as Bytes (ByteString, unpack, pack)
import qualified Data.ByteString.Lazy as LazyBytes (ByteString, unpack, pack)
import qualified Data.ByteString.Short as ShortBytes (ShortByteString, unpack, pack)
import qualified Data.Text as Text (Text, unpack, pack)
import qualified Data.Text.Lazy as LazyText (Text, unpack, pack)
import Data.Vector (Vector)
import qualified Data.Vector.Strict as StrictVector (Vector)
import qualified Data.Vector.Unboxed as UnboxedVector (Unbox, Vector, fromList, toList)
import qualified Data.Vector.Storable as StorableVector (Storable, Vector, fromList, toList)

-- Package.
import Data.MonoFunctor (MonoFunctor (..))
import Data.MonoFoldable (MonoFoldable)


{- | Typeclass for monomorphic containers that can be traversed from beginning to end. -}
class MonoFoldable f => MonoTraversable f where
    {-# MINIMAL monotraverse #-}

    {- | Map each element of a monomorphic container to an action, evaluate them and collect the results. -}
    monotraverse :: Applicative m => (ElementOf f -> m (ElementOf f)) -> f -> m f


-- Instances.
instance MonoTraversable Bytes.ByteString where
    monotraverse :: Applicative m => (Word8 -> m Word8) -> Bytes.ByteString -> m Bytes.ByteString
    monotraverse f = fmap Bytes.pack . traverse f . Bytes.unpack

instance MonoTraversable LazyBytes.ByteString where
    monotraverse :: Applicative m => (Word8 -> m Word8) -> LazyBytes.ByteString -> m LazyBytes.ByteString
    monotraverse f = fmap LazyBytes.pack . traverse f . LazyBytes.unpack

instance MonoTraversable ShortBytes.ShortByteString where
    monotraverse :: Applicative m => (Word8 -> m Word8) -> ShortBytes.ShortByteString -> m ShortBytes.ShortByteString
    monotraverse f = fmap ShortBytes.pack . traverse f . ShortBytes.unpack

instance MonoTraversable Text.Text where
    monotraverse :: Applicative m => (Char -> m Char) -> Text.Text -> m Text.Text
    monotraverse f = fmap Text.pack . traverse f . Text.unpack

instance MonoTraversable LazyText.Text where
    monotraverse :: Applicative m => (Char -> m Char) -> LazyText.Text -> m LazyText.Text
    monotraverse f = fmap LazyText.pack . traverse f . LazyText.unpack

instance MonoTraversable [a] where
    monotraverse :: Applicative m => (a -> m a) -> [a] -> m [a]
    monotraverse = traverse

instance MonoTraversable (NonEmpty a) where
    monotraverse :: Applicative m => (a -> m a) -> NonEmpty a -> m (NonEmpty a)
    monotraverse = traverse

instance MonoTraversable (Maybe a) where
    monotraverse :: Applicative m => (a -> m a) -> Maybe a -> m (Maybe a)
    monotraverse = traverse

instance MonoTraversable (Either e a) where
    monotraverse :: Applicative m => (a -> m a) -> Either e a -> m (Either e a)
    monotraverse = traverse

instance MonoTraversable (Seq a) where
    monotraverse :: Applicative m => (a -> m a) -> Seq a -> m (Seq a)
    monotraverse = traverse

instance MonoTraversable (Vector a) where
    monotraverse :: Applicative m => (a -> m a) -> Vector a -> m (Vector a)
    monotraverse = traverse

instance MonoTraversable (StrictVector.Vector a) where
    monotraverse :: Applicative m => (a -> m a) -> StrictVector.Vector a -> m (StrictVector.Vector a)
    monotraverse = traverse

instance UnboxedVector.Unbox a => MonoTraversable (UnboxedVector.Vector a) where
    monotraverse :: Applicative m => (a -> m a) -> UnboxedVector.Vector a -> m (UnboxedVector.Vector a)
    monotraverse f = fmap UnboxedVector.fromList . traverse f . UnboxedVector.toList

instance StorableVector.Storable a => MonoTraversable (StorableVector.Vector a) where
    monotraverse :: Applicative m => (a -> m a) -> StorableVector.Vector a -> m (StorableVector.Vector a)
    monotraverse f = fmap StorableVector.fromList . traverse f . StorableVector.toList
