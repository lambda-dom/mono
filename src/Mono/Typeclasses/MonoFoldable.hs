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
import qualified Data.ByteString.Lazy as LazyBytes (ByteString, unpack, null, length, foldr, foldl', elem)
import qualified Data.ByteString.Short as ShortBytes (ShortByteString, unpack, null, length, foldr, foldl', elem)
import qualified Data.Text as Text (Text, unpack, null, length, foldr, foldl', elem)
import qualified Data.Text.Lazy as LazyText (Text, unpack, null, length, foldr, foldl', elem)
import Data.Sequence (Seq)
import Data.Vector (Vector)
import qualified Data.Vector.Strict as StrictVector (Vector)
import qualified Data.Vector.Unboxed as UnboxedVector (Vector, Unbox, toList, null, length, foldMap', foldl', foldr, elem)
import qualified Data.Vector.Storable as StorableVector (Vector, Storable, toList, null, length, foldMap', foldl', foldr, elem)

-- Package.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))
import Mono.Typeclasses.MonoPointed (MonoPointed (..))


{- | The typeclass for monofunctors that can be folded over, the monomorphic version of 'Foldable'.

All methods have a default implementation in terms of 'monotoList' and it is implicitly assumed
that any overriding definitions are extensionally equal to the default ones. Given this assumption:

__Mononaturality__: 'monotoList' :: f -> ['ElementOf' f] is mononatural.

Furthermore, 'monotoList' must be an extension of 'monopoint':

__Purity__: We have @'monotoList' . 'monopoint' == 'monopoint'@.
-}
class MonoPointed f => MonoFoldable f where
    {-# MINIMAL monotoList #-}

    {- | Convert a monofoldable to a list. -}
    monotoList :: f -> [ElementOf f]

    {- | Strict fold with a function @'Monoid' m => 'ElementOf' f -> m@. -}
    monofoldMap :: Monoid m => (ElementOf f -> m) -> f -> m
    monofoldMap f = foldl' (<>) mempty . fmap f . monotoList

    {- | Lazy right-associative fold of a monofoldable. -}
    monofoldr :: (ElementOf f -> a -> a) -> a -> f -> a
    monofoldr f x = foldr f x . monotoList

    {- | Right-associative, strict in the accumulator, fold of a monofoldable. -}
    monofoldl :: (a -> ElementOf f -> a) -> a -> f -> a
    monofoldl f x = foldl' f x . monotoList

    {- | Return true if the monofoldable has no elements. -}
    mononull :: f -> Bool
    mononull = null . monotoList

    {- | The number of elements in the monofoldable. -}
    monolength :: f -> Word
    monolength = fromIntegral . length . monotoList

    {- | Return True if @elem@ is an element of the monofoldable. -}
    monoelem :: Eq (ElementOf f) => ElementOf f -> f -> Bool
    monoelem x = (x `elem`) . monotoList


-- Monofoldable instances.
instance MonoFoldable Bytes.ByteString where
    monotoList :: Bytes.ByteString -> [Word8]
    monotoList = Bytes.unpack

    monofoldr :: (Word8 -> a -> a) -> a -> Bytes.ByteString -> a
    monofoldr = Bytes.foldr

    monofoldl :: (a -> Word8 -> a) -> a -> Bytes.ByteString -> a
    monofoldl = Bytes.foldl'

    mononull :: Bytes.ByteString -> Bool
    mononull = Bytes.null

    monolength :: Bytes.ByteString -> Word
    monolength = fromIntegral . Bytes.length

    monoelem :: Word8 -> Bytes.ByteString -> Bool
    monoelem = Bytes.elem

instance MonoFoldable LazyBytes.ByteString where
    monotoList :: LazyBytes.ByteString -> [Word8]
    monotoList = LazyBytes.unpack

    monofoldr :: (Word8 -> a -> a) -> a -> LazyBytes.ByteString -> a
    monofoldr = LazyBytes.foldr

    monofoldl :: (a -> Word8 -> a) -> a -> LazyBytes.ByteString -> a
    monofoldl = LazyBytes.foldl'

    mononull :: LazyBytes.ByteString -> Bool
    mononull = LazyBytes.null

    monolength :: LazyBytes.ByteString -> Word
    monolength = fromIntegral . LazyBytes.length

    monoelem :: Word8 -> LazyBytes.ByteString -> Bool
    monoelem = LazyBytes.elem

instance MonoFoldable ShortBytes.ShortByteString where
    monotoList :: ShortBytes.ShortByteString -> [Word8]
    monotoList = ShortBytes.unpack

    monofoldr :: (Word8 -> a -> a) -> a -> ShortBytes.ShortByteString -> a
    monofoldr = ShortBytes.foldr

    monofoldl :: (a -> Word8 -> a) -> a -> ShortBytes.ShortByteString -> a
    monofoldl = ShortBytes.foldl'

    mononull :: ShortBytes.ShortByteString -> Bool
    mononull = ShortBytes.null

    monolength :: ShortBytes.ShortByteString -> Word
    monolength = fromIntegral . ShortBytes.length

    monoelem :: Word8 -> ShortBytes.ShortByteString -> Bool
    monoelem = ShortBytes.elem

instance MonoFoldable Text.Text where
    monotoList :: Text.Text -> [Char]
    monotoList = Text.unpack

    monofoldr :: (Char -> a -> a) -> a -> Text.Text -> a
    monofoldr = Text.foldr

    monofoldl :: (a -> Char -> a) -> a -> Text.Text -> a
    monofoldl = Text.foldl'

    mononull :: Text.Text -> Bool
    mononull = Text.null

    monolength :: Text.Text -> Word
    monolength = fromIntegral . Text.length

    monoelem :: Char -> Text.Text -> Bool
    monoelem = Text.elem

instance MonoFoldable LazyText.Text where
    monotoList :: LazyText.Text -> [Char]
    monotoList = LazyText.unpack

    monofoldr :: (Char -> a -> a) -> a -> LazyText.Text -> a
    monofoldr = LazyText.foldr

    monofoldl :: (a -> Char -> a) -> a -> LazyText.Text -> a
    monofoldl = LazyText.foldl'

    mononull :: LazyText.Text -> Bool
    mononull = LazyText.null

    monolength :: LazyText.Text -> Word
    monolength = fromIntegral . LazyText.length

    monoelem :: Char -> LazyText.Text -> Bool
    monoelem = LazyText.elem

instance MonoFoldable [a] where
    monotoList :: [a] -> [a]
    monotoList = id

    monofoldMap :: Monoid m => (a -> m) -> [a] -> m
    monofoldMap f = foldl' (<>) mempty . fmap f

    monofoldr :: (a -> b -> b) -> b -> [a] -> b
    monofoldr = foldr

    monofoldl :: (b -> a -> b) -> b -> [a] -> b
    monofoldl = foldl'

    mononull :: [a] -> Bool
    mononull = null

    monolength :: [a] -> Word
    monolength = fromIntegral . length

    monoelem :: Eq a => a -> [a] -> Bool
    monoelem = elem

instance MonoFoldable (NonEmpty a) where
    monotoList :: NonEmpty a -> [a]
    monotoList = toList

    monofoldr :: (a -> b -> b) -> b -> NonEmpty a -> b
    monofoldr = foldr

    monofoldl :: (b -> a -> b) -> b -> NonEmpty a -> b
    monofoldl = foldl'

    mononull :: NonEmpty a -> Bool
    mononull = null

    monolength :: NonEmpty a -> Word
    monolength = fromIntegral . length

    monoelem :: Eq a => a -> NonEmpty a -> Bool
    monoelem = elem

instance MonoFoldable (Maybe a) where
    monotoList :: Maybe a -> [a]
    monotoList = maybe [] singleton

    monofoldMap :: Monoid m => (a -> m) -> Maybe a -> m
    monofoldMap = maybe mempty

    monofoldr :: (a -> b -> b) -> b -> Maybe a -> b
    monofoldr = foldr

    monofoldl :: (b -> a -> b) -> b -> Maybe a -> b
    monofoldl = foldl'

    mononull :: Maybe a -> Bool
    mononull = isNothing

    monolength :: Maybe a -> Word
    monolength = maybe 0 (const 1)

    monoelem :: Eq a => a -> Maybe a -> Bool
    monoelem x = (Just x ==)

instance MonoFoldable (Either e a) where
    monotoList :: Either e a -> [a]
    monotoList = either (const []) singleton

    monofoldMap :: Monoid m => (a -> m) -> Either e a -> m
    monofoldMap = either (const mempty)

    monofoldr :: (a -> b -> b) -> b -> Either e a -> b
    monofoldr = foldr

    monofoldl :: (b -> a -> b) -> b -> Either e a -> b
    monofoldl = foldl'

    mononull :: Either e a -> Bool
    mononull = either (const True) (const False)

    monolength :: Either e a -> Word
    monolength = either (const 0) (const 1)

    monoelem :: Eq a => a -> Either e a -> Bool
    monoelem x = either (const False) (x ==)

instance MonoFoldable (Seq a) where
    monotoList :: Seq a -> [a]
    monotoList = toList

    monofoldr :: (a -> b -> b) -> b -> Seq a -> b
    monofoldr = foldr

    monofoldl :: (b -> a -> b) -> b -> Seq a -> b
    monofoldl = foldl'

    mononull :: Seq a -> Bool
    mononull = null

    monolength :: Seq a -> Word
    monolength = fromIntegral . length

    monoelem :: Eq a => a -> Seq a -> Bool
    monoelem = elem

instance MonoFoldable (Vector a) where
    monotoList :: Vector a -> [a]
    monotoList = toList

    monofoldr :: (a -> b -> b) -> b -> Vector a -> b
    monofoldr = foldr

    monofoldl :: (b -> a -> b) -> b -> Vector a -> b
    monofoldl = foldl'

    mononull :: Vector a -> Bool
    mononull = null

    monolength :: Vector a -> Word
    monolength = fromIntegral . length

    monoelem :: Eq a => a -> Vector a -> Bool
    monoelem = elem

instance MonoFoldable (StrictVector.Vector a) where
    monotoList :: StrictVector.Vector a -> [a]
    monotoList = toList

    monofoldr :: (a -> b -> b) -> b -> StrictVector.Vector a -> b
    monofoldr = foldr

    monofoldl :: (b -> a -> b) -> b -> StrictVector.Vector a -> b
    monofoldl = foldl'

    mononull :: StrictVector.Vector a -> Bool
    mononull = null

    monolength :: StrictVector.Vector a -> Word
    monolength = fromIntegral . length

    monoelem :: Eq a => a -> StrictVector.Vector a -> Bool
    monoelem = elem

instance UnboxedVector.Unbox a => MonoFoldable (UnboxedVector.Vector a) where
    monotoList :: UnboxedVector.Vector a -> [a]
    monotoList = UnboxedVector.toList

    monofoldMap :: Monoid m => (a -> m) -> UnboxedVector.Vector a -> m
    monofoldMap = UnboxedVector.foldMap'

    monofoldr :: (a -> b -> b) -> b -> UnboxedVector.Vector a -> b
    monofoldr = UnboxedVector.foldr

    monofoldl :: (b -> a -> b) -> b -> UnboxedVector.Vector a -> b
    monofoldl = UnboxedVector.foldl'

    mononull :: UnboxedVector.Vector a -> Bool
    mononull = UnboxedVector.null

    monolength :: UnboxedVector.Vector a -> Word
    monolength = fromIntegral . UnboxedVector.length

    monoelem :: Eq a => a -> UnboxedVector.Vector a -> Bool
    monoelem = UnboxedVector.elem

instance StorableVector.Storable a => MonoFoldable (StorableVector.Vector a) where
    monotoList :: StorableVector.Vector a -> [a]
    monotoList = StorableVector.toList

    monofoldMap :: Monoid m => (a -> m) -> StorableVector.Vector a -> m
    monofoldMap = StorableVector.foldMap'

    monofoldr :: (a -> b -> b) -> b -> StorableVector.Vector a -> b
    monofoldr = StorableVector.foldr

    monofoldl :: (b -> a -> b) -> b -> StorableVector.Vector a -> b
    monofoldl = StorableVector.foldl'

    mononull :: StorableVector.Vector a -> Bool
    mononull = StorableVector.null

    monolength :: StorableVector.Vector a -> Word
    monolength = fromIntegral . StorableVector.length

    monoelem :: Eq a => a -> StorableVector.Vector a -> Bool
    monoelem = StorableVector.elem
