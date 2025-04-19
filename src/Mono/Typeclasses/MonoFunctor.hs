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
import qualified Data.ByteString.Lazy as LBytes (ByteString, map)
import qualified Data.ByteString.Short as SBytes (ShortByteString, map)
import qualified Data.Text as Text (Text, map)
import qualified Data.Text.Lazy as LText (Text, map)
import Data.Sequence (Seq)
import Data.Vector (Vector)
import qualified Data.Vector.Strict as SVector (Vector)
import qualified Data.Vector.Unboxed as UVector (Vector, Unbox, map)
import qualified Data.Vector.Storable as StVector (Vector, Storable, map)
import qualified Data.Array.IArray as Array (Array)


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

instance MonoFunctor LBytes.ByteString where
    type ElementOf LBytes.ByteString = Word8

    monomap :: (Word8 -> Word8) -> LBytes.ByteString -> LBytes.ByteString
    monomap = LBytes.map

instance MonoFunctor SBytes.ShortByteString where
    type ElementOf SBytes.ShortByteString = Word8

    monomap :: (Word8 -> Word8) -> SBytes.ShortByteString -> SBytes.ShortByteString
    monomap = SBytes.map

instance MonoFunctor Text.Text where
    type ElementOf Text.Text = Char

    monomap :: (Char -> Char) -> Text.Text -> Text.Text
    monomap = Text.map

instance MonoFunctor LText.Text where
    type ElementOf LText.Text = Char

    monomap :: (Char -> Char) -> LText.Text -> LText.Text
    monomap = LText.map

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

instance MonoFunctor (SVector.Vector a) where
    type ElementOf (SVector.Vector a) = a

    monomap :: (a -> a) -> SVector.Vector a -> SVector.Vector a
    monomap = fmap

instance UVector.Unbox a => MonoFunctor (UVector.Vector a) where
    type ElementOf (UVector.Vector a) = a

    monomap :: (a -> a) -> UVector.Vector a -> UVector.Vector a
    monomap = UVector.map

instance StVector.Storable a => MonoFunctor (StVector.Vector a) where
    type ElementOf (StVector.Vector a) = a

    monomap :: (a -> a) -> StVector.Vector a -> StVector.Vector a
    monomap = StVector.map

instance MonoFunctor (Array.Array i a) where
    type ElementOf (Array.Array i a) = a

    monomap :: (a -> a) -> Array.Array i a -> Array.Array i a
    monomap = fmap
