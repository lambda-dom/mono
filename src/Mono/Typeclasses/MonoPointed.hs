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
import Mono.Typeclasses.MonoFunctor (MonoFunctor)


{- | The @MonoPointed@ typeclass.

There is one law that the typeclass must satisfy:

__Mononaturality__: @'monopoint' :: a -> f@ is mononatural. Specifically, we
have for all @g :: a -> a@:

@
    monomap g . monopoint == monopoint . g
@
-}
class MonoFunctor a f => MonoPointed a f where
    {- | Embed an element in the 'MonoFunctor'. -}
    monopoint :: a -> f


-- Instances.
instance MonoPointed Word8 Bytes.ByteString where
    {-# INLINE monopoint #-}
    monopoint :: Word8 -> Bytes.ByteString
    monopoint = Bytes.singleton

instance MonoPointed Word8 LBytes.ByteString where
    {-# INLINE monopoint #-}
    monopoint :: Word8 -> LBytes.ByteString
    monopoint = LBytes.singleton

instance MonoPointed Word8 SBytes.ShortByteString where
    {-# INLINE monopoint #-}
    monopoint :: Word8 -> SBytes.ShortByteString
    monopoint = SBytes.singleton

instance MonoPointed Char Text.Text where
    {-# INLINE monopoint #-}
    monopoint :: Char -> Text.Text
    monopoint = Text.singleton

instance MonoPointed Char LText.Text where
    {-# INLINE monopoint #-}
    monopoint :: Char -> LText.Text
    monopoint = LText.singleton


-- Functor instances.
instance MonoPointed a [a] where
    {-# INLINE monopoint #-}
    monopoint :: a -> [a]
    monopoint = pure

instance MonoPointed a (NonEmpty a) where
    {-# INLINE monopoint #-}
    monopoint :: a -> NonEmpty a
    monopoint = pure

instance MonoPointed a (Maybe a) where
    {-# INLINE monopoint #-}
    monopoint :: a -> Maybe a
    monopoint = pure

instance MonoPointed a (Either e a) where
    {-# INLINE monopoint #-}
    monopoint :: a -> Either e a
    monopoint = pure

instance MonoPointed a (Seq a) where
    {-# INLINE monopoint #-}
    monopoint :: a -> Seq a
    monopoint = pure

instance MonoPointed a (Vector a) where
    {-# INLINE monopoint #-}
    monopoint :: a -> Vector a
    monopoint = pure

instance MonoPointed a (SVector.Vector a) where
    {-# INLINE monopoint #-}
    monopoint :: a -> SVector.Vector a
    monopoint = pure

instance UVector.Unbox a => MonoPointed a (UVector.Vector a) where
    {-# INLINE monopoint #-}
    monopoint :: a -> UVector.Vector a
    monopoint = UVector.singleton

instance StVector.Storable a => MonoPointed a (StVector.Vector a) where
    {-# INLINE monopoint #-}
    monopoint :: a -> StVector.Vector a
    monopoint = StVector.singleton
