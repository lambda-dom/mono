{- |
Module: Mono.Typeclasses.MonoTraversable

The @MonoTraversable@ typeclass.
-}

module Mono.Typeclasses.MonoTraversable (
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
import qualified Data.ByteString.Lazy as LBytes (ByteString, unpack, pack)
import qualified Data.ByteString.Short as SBytes (ShortByteString, unpack, pack)
import qualified Data.Text as Text (Text, unpack, pack)
import qualified Data.Text.Lazy as LText (Text, unpack, pack)
import Data.Vector (Vector)
import qualified Data.Vector.Strict as SVector (Vector)
import qualified Data.Vector.Unboxed as UVector (Unbox, Vector, fromList, toList)
import qualified Data.Vector.Storable as StVector (Storable, Vector, fromList, toList)

-- Package.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))
import Mono.Typeclasses.MonoFoldable (MonoFoldable)


{- | Typeclass for monomorphic containers that can be traversed from beginning to end. -}
class MonoFoldable f => MonoTraversable f where
    {-# MINIMAL monotraverse #-}

    {- | Map each element of a monomorphic container to an action, evaluate them and collect the results. -}
    monotraverse :: Applicative m => (ElementOf f -> m (ElementOf f)) -> f -> m f


-- Instances.
instance MonoTraversable Bytes.ByteString where
    {-# INLINEABLE monotraverse #-}
    monotraverse :: Applicative m => (Word8 -> m Word8) -> Bytes.ByteString -> m Bytes.ByteString
    monotraverse f = fmap Bytes.pack . traverse f . Bytes.unpack

instance MonoTraversable LBytes.ByteString where
    {-# INLINEABLE monotraverse #-}
    monotraverse :: Applicative m => (Word8 -> m Word8) -> LBytes.ByteString -> m LBytes.ByteString
    monotraverse f = fmap LBytes.pack . traverse f . LBytes.unpack

instance MonoTraversable SBytes.ShortByteString where
    {-# INLINEABLE monotraverse #-}
    monotraverse :: Applicative m => (Word8 -> m Word8) -> SBytes.ShortByteString -> m SBytes.ShortByteString
    monotraverse f = fmap SBytes.pack . traverse f . SBytes.unpack

instance MonoTraversable Text.Text where
    {-# INLINEABLE monotraverse #-}
    monotraverse :: Applicative m => (Char -> m Char) -> Text.Text -> m Text.Text
    monotraverse f = fmap Text.pack . traverse f . Text.unpack

instance MonoTraversable LText.Text where
    {-# INLINEABLE monotraverse #-}
    monotraverse :: Applicative m => (Char -> m Char) -> LText.Text -> m LText.Text
    monotraverse f = fmap LText.pack . traverse f . LText.unpack

instance MonoTraversable [a] where
    {-# INLINE monotraverse #-}
    monotraverse :: Applicative m => (a -> m a) -> [a] -> m [a]
    monotraverse = traverse

instance MonoTraversable (NonEmpty a) where
    {-# INLINE monotraverse #-}
    monotraverse :: Applicative m => (a -> m a) -> NonEmpty a -> m (NonEmpty a)
    monotraverse = traverse

instance MonoTraversable (Maybe a) where
    {-# INLINE monotraverse #-}
    monotraverse :: Applicative m => (a -> m a) -> Maybe a -> m (Maybe a)
    monotraverse = traverse

instance MonoTraversable (Either e a) where
    {-# INLINE monotraverse #-}
    monotraverse :: Applicative m => (a -> m a) -> Either e a -> m (Either e a)
    monotraverse = traverse

instance MonoTraversable (Seq a) where
    {-# INLINE monotraverse #-}
    monotraverse :: Applicative m => (a -> m a) -> Seq a -> m (Seq a)
    monotraverse = traverse

instance MonoTraversable (Vector a) where
    {-# INLINE monotraverse #-}
    monotraverse :: Applicative m => (a -> m a) -> Vector a -> m (Vector a)
    monotraverse = traverse

instance MonoTraversable (SVector.Vector a) where
    {-# INLINE monotraverse #-}
    monotraverse :: Applicative m => (a -> m a) -> SVector.Vector a -> m (SVector.Vector a)
    monotraverse = traverse

instance UVector.Unbox a => MonoTraversable (UVector.Vector a) where
    {-# INLINEABLE monotraverse #-}
    monotraverse :: Applicative m => (a -> m a) -> UVector.Vector a -> m (UVector.Vector a)
    monotraverse f = fmap UVector.fromList . traverse f . UVector.toList

instance StVector.Storable a => MonoTraversable (StVector.Vector a) where
    {-# INLINEABLE monotraverse #-}
    monotraverse :: Applicative m => (a -> m a) -> StVector.Vector a -> m (StVector.Vector a)
    monotraverse f = fmap StVector.fromList . traverse f . StVector.toList
