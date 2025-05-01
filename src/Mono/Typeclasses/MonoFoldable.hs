{- |
Module: Mono.Typeclasses.MonoFoldable

The @MonoFoldable@ typeclass.
-}

module Mono.Typeclasses.MonoFoldable (
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
import qualified Data.ByteString as Bytes (ByteString, unpack, null, length, foldr, foldl', elem)
import qualified Data.ByteString.Lazy as LBytes (ByteString, unpack, null, length, foldr, foldl', elem)
import qualified Data.ByteString.Short as SBytes (ShortByteString, unpack, null, length, foldr, foldl', elem)
import qualified Data.Text as Text (Text, unpack, null, length, foldr, foldl', elem)
import qualified Data.Text.Lazy as LText (Text, unpack, null, length, foldr, foldl', elem)
import Data.Sequence (Seq)
import Data.Vector (Vector)
import qualified Data.Vector.Strict as SVector (Vector)
import qualified Data.Vector.Unboxed as UVector (Vector, Unbox, toList, null, length, foldMap', foldl', foldr, elem)
import qualified Data.Vector.Storable as StVector (Vector, Storable, toList, null, length, foldMap', foldl', foldr, elem)

-- Package.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))


{- | The typeclass for monofunctors that can be folded over, the monomorphic version of 'Foldable'.

All methods have a default implementation in terms of 'monotoList' and it is implicitly assumed
that any overriding definitions are extensionally equal to the default ones. Given this assumption:

__Mononaturality__: @'monotoList' :: f -> ['ElementOf' f]@ is mononatural.

Furthermore, 'monotoList' must be an extension of 'Mono.Typeclasses.MonoPointed.monopoint':

__Purity__: For @'Mono.Typeclasses.MonoPointed' f@, we must have:

@
'monotoList' . 'Mono.Typeclasses.MonoPointed.monopoint' == 'Mono.Typeclasses.MonoPointed.monopoint'
@
-}
class MonoFunctor f => MonoFoldable f where
    {-# MINIMAL monotoList #-}

    {- | Convert a monofoldable to a list. -}
    monotoList :: f -> [ElementOf f]

    {- | Strict fold with a function @'Monoid' m => 'ElementOf' f -> m@. -}
    {-# INLINEABLE monofoldMap #-}
    monofoldMap :: Monoid m => (ElementOf f -> m) -> f -> m
    monofoldMap f = foldl' (<>) mempty . fmap f . monotoList

    {- | Lazy right-associative fold of a monofoldable. -}
    {-# INLINEABLE monofoldr #-}
    monofoldr :: (ElementOf f -> a -> a) -> a -> f -> a
    monofoldr f x = foldr f x . monotoList

    {- | Right-associative, strict in the accumulator, fold of a monofoldable. -}
    {-# INLINEABLE monofoldl #-}
    monofoldl :: (a -> ElementOf f -> a) -> a -> f -> a
    monofoldl f x = foldl' f x . monotoList

    {- | Return true if the monofoldable has no elements. -}
    {-# INLINEABLE mononull #-}
    mononull :: f -> Bool
    mononull = null . monotoList

    {- | The number of elements in the monofoldable. -}
    {-# INLINEABLE monolength #-}
    monolength :: f -> Word
    monolength = fromIntegral . length . monotoList

    {- | Return True if @elem@ is an element of the monofoldable. -}
    {-# INLINEABLE monoelem #-}
    monoelem :: Eq (ElementOf f) => ElementOf f -> f -> Bool
    monoelem x = (x `elem`) . monotoList


-- Monofoldable instances.
instance MonoFoldable Bytes.ByteString where
    {-# INLINE monotoList #-}
    monotoList :: Bytes.ByteString -> [Word8]
    monotoList = Bytes.unpack

    {-# INLINE monofoldr #-}
    monofoldr :: (Word8 -> a -> a) -> a -> Bytes.ByteString -> a
    monofoldr = Bytes.foldr

    {-# INLINE monofoldl #-}
    monofoldl :: (a -> Word8 -> a) -> a -> Bytes.ByteString -> a
    monofoldl = Bytes.foldl'

    {-# INLINE mononull #-}
    mononull :: Bytes.ByteString -> Bool
    mononull = Bytes.null

    {-# INLINE monolength #-}
    monolength :: Bytes.ByteString -> Word
    monolength = fromIntegral . Bytes.length

    {-# INLINE monoelem #-}
    monoelem :: Word8 -> Bytes.ByteString -> Bool
    monoelem = Bytes.elem

instance MonoFoldable LBytes.ByteString where
    {-# INLINE monotoList #-}
    monotoList :: LBytes.ByteString -> [Word8]
    monotoList = LBytes.unpack

    {-# INLINE monofoldr #-}
    monofoldr :: (Word8 -> a -> a) -> a -> LBytes.ByteString -> a
    monofoldr = LBytes.foldr

    {-# INLINE monofoldl #-}
    monofoldl :: (a -> Word8 -> a) -> a -> LBytes.ByteString -> a
    monofoldl = LBytes.foldl'

    {-# INLINE mononull #-}
    mononull :: LBytes.ByteString -> Bool
    mononull = LBytes.null

    {-# INLINE monolength #-}
    monolength :: LBytes.ByteString -> Word
    monolength = fromIntegral . LBytes.length

    {-# INLINE monoelem #-}
    monoelem :: Word8 -> LBytes.ByteString -> Bool
    monoelem = LBytes.elem

instance MonoFoldable SBytes.ShortByteString where
    {-# INLINE monotoList #-}
    monotoList :: SBytes.ShortByteString -> [Word8]
    monotoList = SBytes.unpack

    {-# INLINE monofoldr #-}
    monofoldr :: (Word8 -> a -> a) -> a -> SBytes.ShortByteString -> a
    monofoldr = SBytes.foldr

    {-# INLINE monofoldl #-}
    monofoldl :: (a -> Word8 -> a) -> a -> SBytes.ShortByteString -> a
    monofoldl = SBytes.foldl'

    {-# INLINE mononull #-}
    mononull :: SBytes.ShortByteString -> Bool
    mononull = SBytes.null

    {-# INLINE monolength #-}
    monolength :: SBytes.ShortByteString -> Word
    monolength = fromIntegral . SBytes.length

    {-# INLINE monoelem #-}
    monoelem :: Word8 -> SBytes.ShortByteString -> Bool
    monoelem = SBytes.elem

instance MonoFoldable Text.Text where
    {-# INLINE monotoList #-}
    monotoList :: Text.Text -> [Char]
    monotoList = Text.unpack

    {-# INLINE monofoldr #-}
    monofoldr :: (Char -> a -> a) -> a -> Text.Text -> a
    monofoldr = Text.foldr

    {-# INLINE monofoldl #-}
    monofoldl :: (a -> Char -> a) -> a -> Text.Text -> a
    monofoldl = Text.foldl'

    {-# INLINE mononull #-}
    mononull :: Text.Text -> Bool
    mononull = Text.null

    {-# INLINE monolength #-}
    monolength :: Text.Text -> Word
    monolength = fromIntegral . Text.length

    {-# INLINE monoelem #-}
    monoelem :: Char -> Text.Text -> Bool
    monoelem = Text.elem

instance MonoFoldable LText.Text where
    {-# INLINE monotoList #-}
    monotoList :: LText.Text -> [Char]
    monotoList = LText.unpack

    {-# INLINE monofoldr #-}
    monofoldr :: (Char -> a -> a) -> a -> LText.Text -> a
    monofoldr = LText.foldr

    {-# INLINE monofoldl #-}
    monofoldl :: (a -> Char -> a) -> a -> LText.Text -> a
    monofoldl = LText.foldl'

    {-# INLINE mononull #-}
    mononull :: LText.Text -> Bool
    mononull = LText.null

    {-# INLINE monolength #-}
    monolength :: LText.Text -> Word
    monolength = fromIntegral . LText.length

    {-# INLINE monoelem #-}
    monoelem :: Char -> LText.Text -> Bool
    monoelem = LText.elem

instance MonoFoldable [a] where
    {-# INLINE monotoList #-}
    monotoList :: [a] -> [a]
    monotoList = id

    {-# INLINEABLE monofoldMap #-}
    monofoldMap :: Monoid m => (a -> m) -> [a] -> m
    monofoldMap f = foldl' (<>) mempty . fmap f

    {-# INLINE monofoldr #-}
    monofoldr :: (a -> b -> b) -> b -> [a] -> b
    monofoldr = foldr

    {-# INLINE monofoldl #-}
    monofoldl :: (b -> a -> b) -> b -> [a] -> b
    monofoldl = foldl'

    {-# INLINE mononull #-}
    mononull :: [a] -> Bool
    mononull = null

    {-# INLINE monolength #-}
    monolength :: [a] -> Word
    monolength = fromIntegral . length

    {-# INLINE monoelem #-}
    monoelem :: Eq a => a -> [a] -> Bool
    monoelem = elem

instance MonoFoldable (NonEmpty a) where
    {-# INLINE monotoList #-}
    monotoList :: NonEmpty a -> [a]
    monotoList = toList

    {-# INLINE monofoldr #-}
    monofoldr :: (a -> b -> b) -> b -> NonEmpty a -> b
    monofoldr = foldr

    {-# INLINE monofoldl #-}
    monofoldl :: (b -> a -> b) -> b -> NonEmpty a -> b
    monofoldl = foldl'

    {-# INLINE mononull #-}
    mononull :: NonEmpty a -> Bool
    mononull = null

    {-# INLINE monolength #-}
    monolength :: NonEmpty a -> Word
    monolength = fromIntegral . length

    {-# INLINE monoelem #-}
    monoelem :: Eq a => a -> NonEmpty a -> Bool
    monoelem = elem

instance MonoFoldable (Maybe a) where
    {-# INLINE monotoList #-}
    monotoList :: Maybe a -> [a]
    monotoList = maybe [] singleton

    {-# INLINEABLE monofoldMap #-}
    monofoldMap :: Monoid m => (a -> m) -> Maybe a -> m
    monofoldMap = maybe mempty

    {-# INLINE monofoldr #-}
    monofoldr :: (a -> b -> b) -> b -> Maybe a -> b
    monofoldr = foldr

    {-# INLINE monofoldl #-}
    monofoldl :: (b -> a -> b) -> b -> Maybe a -> b
    monofoldl = foldl'

    {-# INLINE mononull #-}
    mononull :: Maybe a -> Bool
    mononull = isNothing

    {-# INLINE monolength #-}
    monolength :: Maybe a -> Word
    monolength = maybe 0 (const 1)

    {-# INLINE monoelem #-}
    monoelem :: Eq a => a -> Maybe a -> Bool
    monoelem x = (Just x ==)

instance MonoFoldable (Either e a) where
    {-# INLINE monotoList #-}
    monotoList :: Either e a -> [a]
    monotoList = either (const []) singleton

    {-# INLINEABLE monofoldMap #-}
    monofoldMap :: Monoid m => (a -> m) -> Either e a -> m
    monofoldMap = either (const mempty)

    {-# INLINE monofoldr #-}
    monofoldr :: (a -> b -> b) -> b -> Either e a -> b
    monofoldr = foldr

    {-# INLINE monofoldl #-}
    monofoldl :: (b -> a -> b) -> b -> Either e a -> b
    monofoldl = foldl'

    {-# INLINE mononull #-}
    mononull :: Either e a -> Bool
    mononull = either (const True) (const False)

    {-# INLINE monolength #-}
    monolength :: Either e a -> Word
    monolength = either (const 0) (const 1)

    {-# INLINE monoelem #-}
    monoelem :: Eq a => a -> Either e a -> Bool
    monoelem x = either (const False) (x ==)

instance MonoFoldable (Seq a) where
    {-# INLINE monotoList #-}
    monotoList :: Seq a -> [a]
    monotoList = toList

    {-# INLINE monofoldr #-}
    monofoldr :: (a -> b -> b) -> b -> Seq a -> b
    monofoldr = foldr

    {-# INLINE monofoldl #-}
    monofoldl :: (b -> a -> b) -> b -> Seq a -> b
    monofoldl = foldl'

    {-# INLINE mononull #-}
    mononull :: Seq a -> Bool
    mononull = null

    {-# INLINE monolength #-}
    monolength :: Seq a -> Word
    monolength = fromIntegral . length

    {-# INLINE monoelem #-}
    monoelem :: Eq a => a -> Seq a -> Bool
    monoelem = elem

instance MonoFoldable (Vector a) where
    {-# INLINE monotoList #-}
    monotoList :: Vector a -> [a]
    monotoList = toList

    {-# INLINE monofoldr #-}
    monofoldr :: (a -> b -> b) -> b -> Vector a -> b
    monofoldr = foldr

    {-# INLINE monofoldl #-}
    monofoldl :: (b -> a -> b) -> b -> Vector a -> b
    monofoldl = foldl'

    {-# INLINE mononull #-}
    mononull :: Vector a -> Bool
    mononull = null

    {-# INLINE monolength #-}
    monolength :: Vector a -> Word
    monolength = fromIntegral . length

    {-# INLINE monoelem #-}
    monoelem :: Eq a => a -> Vector a -> Bool
    monoelem = elem

instance MonoFoldable (SVector.Vector a) where
    {-# INLINE monotoList #-}
    monotoList :: SVector.Vector a -> [a]
    monotoList = toList

    {-# INLINE monofoldr #-}
    monofoldr :: (a -> b -> b) -> b -> SVector.Vector a -> b
    monofoldr = foldr

    {-# INLINE monofoldl #-}
    monofoldl :: (b -> a -> b) -> b -> SVector.Vector a -> b
    monofoldl = foldl'

    {-# INLINE mononull #-}
    mononull :: SVector.Vector a -> Bool
    mononull = null

    {-# INLINE monolength #-}
    monolength :: SVector.Vector a -> Word
    monolength = fromIntegral . length

    {-# INLINE monoelem #-}
    monoelem :: Eq a => a -> SVector.Vector a -> Bool
    monoelem = elem

instance UVector.Unbox a => MonoFoldable (UVector.Vector a) where
    {-# INLINE monotoList #-}
    monotoList :: UVector.Vector a -> [a]
    monotoList = UVector.toList

    {-# INLINEABLE monofoldMap #-}
    monofoldMap :: Monoid m => (a -> m) -> UVector.Vector a -> m
    monofoldMap = UVector.foldMap'

    {-# INLINE monofoldr #-}
    monofoldr :: (a -> b -> b) -> b -> UVector.Vector a -> b
    monofoldr = UVector.foldr

    {-# INLINE monofoldl #-}
    monofoldl :: (b -> a -> b) -> b -> UVector.Vector a -> b
    monofoldl = UVector.foldl'

    {-# INLINE mononull #-}
    mononull :: UVector.Vector a -> Bool
    mononull = UVector.null

    {-# INLINE monolength #-}
    monolength :: UVector.Vector a -> Word
    monolength = fromIntegral . UVector.length

    {-# INLINE monoelem #-}
    monoelem :: Eq a => a -> UVector.Vector a -> Bool
    monoelem = UVector.elem

instance StVector.Storable a => MonoFoldable (StVector.Vector a) where
    {-# INLINE monotoList #-}
    monotoList :: StVector.Vector a -> [a]
    monotoList = StVector.toList

    {-# INLINEABLE monofoldMap #-}
    monofoldMap :: Monoid m => (a -> m) -> StVector.Vector a -> m
    monofoldMap = StVector.foldMap'

    {-# INLINE monofoldr #-}
    monofoldr :: (a -> b -> b) -> b -> StVector.Vector a -> b
    monofoldr = StVector.foldr

    {-# INLINE monofoldl #-}
    monofoldl :: (b -> a -> b) -> b -> StVector.Vector a -> b
    monofoldl = StVector.foldl'

    {-# INLINE mononull #-}
    mononull :: StVector.Vector a -> Bool
    mononull = StVector.null

    {-# INLINE monolength #-}
    monolength :: StVector.Vector a -> Word
    monolength = fromIntegral . StVector.length

    {-# INLINE monoelem #-}
    monoelem :: Eq a => a -> StVector.Vector a -> Bool
    monoelem = StVector.elem
