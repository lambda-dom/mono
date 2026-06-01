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
import Data.Word (Word8)

-- Libraries.
import qualified Data.ByteString as Bytes (ByteString, map)
import qualified Data.ByteString.Lazy as LBytes (ByteString, map)
import qualified Data.ByteString.Short as SBytes (ShortByteString, map)
import qualified Data.Text as Text (Text, map)
import qualified Data.Text.Lazy as LText (Text, map)
import Data.Sequence (Seq)
import Data.Vector (Vector)
import qualified Data.Vector.Strict as SVector (Vector)
import qualified Data.Vector.Unboxed as UVector (Vector, Unbox, map)
import qualified Data.Vector.Storable as StVector (Vector, Storable, map)


{- | The typeclass for pure monofunctors, the monomorphic version of 'Functor'. -}
class MonoFunctor a s | s -> a where
    {-# MINIMAL monomap #-}

    {- | Map over a monofunctor. -}
    monomap :: (a -> a) -> s -> s


-- Functor instances.
-- Base.
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


-- Libraries.
instance MonoFunctor Word8 Bytes.ByteString where
    {-# INLINE monomap #-}
    monomap :: (Word8 -> Word8) -> Bytes.ByteString -> Bytes.ByteString
    monomap = Bytes.map

instance MonoFunctor Word8 LBytes.ByteString where
    {-# INLINE monomap #-}
    monomap :: (Word8 -> Word8) -> LBytes.ByteString -> LBytes.ByteString
    monomap = LBytes.map

instance MonoFunctor Word8 SBytes.ShortByteString where
    {-# INLINE monomap #-}
    monomap :: (Word8 -> Word8) -> SBytes.ShortByteString -> SBytes.ShortByteString
    monomap = SBytes.map

instance MonoFunctor Char Text.Text where
    {-# INLINE monomap #-}
    monomap :: (Char -> Char) -> Text.Text -> Text.Text
    monomap = Text.map

instance MonoFunctor Char LText.Text where
    {-# INLINE monomap #-}
    monomap :: (Char -> Char) -> LText.Text -> LText.Text
    monomap = LText.map

instance MonoFunctor a (Seq a) where
    {-# INLINE monomap #-}
    monomap :: (a -> a) -> Seq a -> Seq a
    monomap = fmap

instance MonoFunctor a (Vector a) where
    {-# INLINE monomap #-}
    monomap :: (a -> a) -> Vector a -> Vector a
    monomap = fmap

instance MonoFunctor a (SVector.Vector a) where
    {-# INLINE monomap #-}
    monomap :: (a -> a) -> SVector.Vector a -> SVector.Vector a
    monomap = fmap

instance UVector.Unbox a => MonoFunctor a (UVector.Vector a) where
    {-# INLINE monomap #-}
    monomap :: (a -> a) -> UVector.Vector a -> UVector.Vector a
    monomap = UVector.map

instance StVector.Storable a => MonoFunctor a (StVector.Vector a) where
    {-# INLINE monomap #-}
    monomap :: (a -> a) -> StVector.Vector a -> StVector.Vector a
    monomap = StVector.map
