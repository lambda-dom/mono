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
import Data.Kind (Type)
import Data.Word (Word8)

-- Libraries.
import qualified Data.ByteString as Bytes (ByteString, map)
import qualified Data.ByteString.Lazy as LazyBytes (ByteString, map)
import qualified Data.ByteString.Short as ShortBytes (ShortByteString, map)
import qualified Data.Text as Text (Text, map)
import qualified Data.Text.Lazy as LazyText (Text, map)
import Data.Sequence (Seq)
import Data.Vector (Vector)
import qualified Data.Vector.Strict as StrictVector (Vector)
import qualified Data.Vector.Unboxed as UnboxedVector (Vector, Unbox, map)
import qualified Data.Vector.Storable as StorableVector (Vector, Storable, map)


{- | The typeclass for monofunctors, the monomorphic version of 'Functor'. -}
class MonoFunctor f where
    {-# MINIMAL monomap #-}

    {- | The type of the elements of the monofunctor. -}
    type ElementOf f :: Type

    {- | Map over a monofunctor. -}
    monomap :: (ElementOf f -> ElementOf f) -> f -> f


-- Monofunctor instances.
instance MonoFunctor Bytes.ByteString where
    type ElementOf Bytes.ByteString = Word8

    monomap :: (Word8 -> Word8) -> Bytes.ByteString -> Bytes.ByteString
    monomap = Bytes.map

instance MonoFunctor LazyBytes.ByteString where
    type ElementOf LazyBytes.ByteString = Word8

    monomap :: (Word8 -> Word8) -> LazyBytes.ByteString -> LazyBytes.ByteString
    monomap = LazyBytes.map

instance MonoFunctor ShortBytes.ShortByteString where
    type ElementOf ShortBytes.ShortByteString = Word8

    monomap :: (Word8 -> Word8) -> ShortBytes.ShortByteString -> ShortBytes.ShortByteString
    monomap = ShortBytes.map

instance MonoFunctor Text.Text where
    type ElementOf Text.Text = Char

    monomap :: (Char -> Char) -> Text.Text -> Text.Text
    monomap = Text.map

instance MonoFunctor LazyText.Text where
    type ElementOf LazyText.Text = Char

    monomap :: (Char -> Char) -> LazyText.Text -> LazyText.Text
    monomap = LazyText.map

-- Functor instances.
instance MonoFunctor [a] where
    type ElementOf [a] = a

    monomap :: (a -> a) -> [a] -> [a]
    monomap = fmap

instance MonoFunctor (NonEmpty a) where
    type ElementOf (NonEmpty a) = a

    monomap :: (a -> a) -> NonEmpty a -> NonEmpty a
    monomap = fmap

instance MonoFunctor (Maybe a) where
    type ElementOf (Maybe a) = a

    monomap :: (a -> a) -> Maybe a -> Maybe a
    monomap = fmap

instance MonoFunctor (Either e a) where
    type ElementOf (Either e a) = a

    monomap :: (a -> a) -> Either e a -> Either e a
    monomap = fmap

instance MonoFunctor (Seq a) where
    type ElementOf (Seq a) = a

    monomap :: (a -> a) -> Seq a -> Seq a
    monomap = fmap

instance MonoFunctor (Vector a) where
    type ElementOf (Vector a) = a

    monomap :: (a -> a) -> Vector a -> Vector a
    monomap = fmap

instance MonoFunctor (StrictVector.Vector a) where
    type ElementOf (StrictVector.Vector a) = a

    monomap :: (a -> a) -> StrictVector.Vector a -> StrictVector.Vector a
    monomap = fmap

instance UnboxedVector.Unbox a => MonoFunctor (UnboxedVector.Vector a) where
    type ElementOf (UnboxedVector.Vector a) = a

    monomap :: (a -> a) -> UnboxedVector.Vector a -> UnboxedVector.Vector a
    monomap = UnboxedVector.map

instance StorableVector.Storable a => MonoFunctor (StorableVector.Vector a) where
    type ElementOf (StorableVector.Vector a) = a

    monomap :: (a -> a) -> StorableVector.Vector a -> StorableVector.Vector a
    monomap = StorableVector.map
