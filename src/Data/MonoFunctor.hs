{- |
Module: Data.MonoFunctor

The @MonoFunctor@ typeclass.
-}

module Data.MonoFunctor (
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
import qualified Data.Text as Text (Text, map)
import qualified Data.Text.Lazy as LazyText (Text, map)
import Data.Sequence (Seq)
import Data.Vector (Vector)


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
