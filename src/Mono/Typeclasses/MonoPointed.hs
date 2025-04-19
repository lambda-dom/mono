{- |
Module: Mono.Typeclasses.MonoPointed

The @MonoPointed@ typeclass.
-}

module Mono.Typeclasses.MonoPointed (
    -- * Typeclasses.
    MonoPointed (..),
) where

-- Imports.
-- Base.
import Data.List.NonEmpty (NonEmpty)
import Data.Word (Word8)

-- Libraries.
import qualified Data.ByteString as Bytes (ByteString, singleton)
import qualified Data.ByteString.Lazy as LBytes (ByteString, singleton)
import qualified Data.ByteString.Short as SBytes (ShortByteString, singleton)
import qualified Data.Text as Text (Text, singleton)
import qualified Data.Text.Lazy as LText (Text, singleton)
import Data.Sequence (Seq)
import Data.Vector (Vector)
import qualified Data.Vector.Strict as SVector (Vector)
import qualified Data.Vector.Unboxed as UVector (Vector, Unbox, singleton)
import qualified Data.Vector.Storable as StVector (Vector, Storable, singleton)

-- Package.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (ElementOf))


{- | The @MonoPointed@ typeclass.

There is one law that the typeclass must satisfy:

__Mononaturality__: @'monopoint' :: f -> ['ElementOf' f]@ is mononatural.
-}
class MonoFunctor f => MonoPointed f where
    {- | Embed an element in the 'MonoFunctor'. -}
    monopoint :: ElementOf f -> f


-- Instances.
instance MonoPointed Bytes.ByteString where
    monopoint :: Word8 -> Bytes.ByteString
    monopoint = Bytes.singleton

instance MonoPointed LBytes.ByteString where
    monopoint :: Word8 -> LBytes.ByteString
    monopoint = LBytes.singleton

instance MonoPointed SBytes.ShortByteString where
    monopoint :: Word8 -> SBytes.ShortByteString
    monopoint = SBytes.singleton

instance MonoPointed Text.Text where
    monopoint :: Char -> Text.Text
    monopoint = Text.singleton

instance MonoPointed LText.Text where
    monopoint :: Char -> LText.Text
    monopoint = LText.singleton


-- Functor instances.
instance MonoPointed [a] where
    monopoint :: a -> [a]
    monopoint = pure

instance MonoPointed (NonEmpty a) where
    monopoint :: a -> NonEmpty a
    monopoint = pure

instance MonoPointed (Maybe a) where
    monopoint :: a -> Maybe a
    monopoint = pure

instance MonoPointed (Either e a) where
    monopoint :: a -> Either e a
    monopoint = pure

instance MonoPointed (Seq a) where
    monopoint :: a -> Seq a
    monopoint = pure

instance MonoPointed (Vector a) where
    monopoint :: a -> Vector a
    monopoint = pure

instance MonoPointed (SVector.Vector a) where
    monopoint :: a -> SVector.Vector a
    monopoint = pure

instance UVector.Unbox a => MonoPointed (UVector.Vector a) where
    monopoint :: a -> UVector.Vector a
    monopoint = UVector.singleton

instance StVector.Storable a => MonoPointed (StVector.Vector a) where
    monopoint :: a -> StVector.Vector a
    monopoint = StVector.singleton
