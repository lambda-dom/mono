{- |
Module: Data.MonoFoldable

The @MonoFoldable@ typeclass.
-}

module Data.MonoFoldable (
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
import Data.MonoFunctor (MonoFunctor (..))


{- | The typeclass for monofunctors that can be folded over, the monomorphic version of 'Foldable'. -}
class MonoFunctor f => MonoFoldable f where
    {-# MINIMAL monotoList #-}

    {- | Convert a monofoldable to a list. -}
    monotoList :: f -> [ElementOf f]

    {- | Strict fold with a function @'Monoid' m => 'ElementOf' f -> m@. -}
    {-# INLINE monofoldMap #-}
    monofoldMap :: Monoid m => (ElementOf f -> m) -> f -> m
    monofoldMap f = foldl' (<>) mempty . fmap f . monotoList

    {- | Lazy right-associative fold of a monofoldable. -}
    {-# INLINE monofoldr #-}
    monofoldr :: (ElementOf f -> a -> a) -> a -> f -> a
    monofoldr f x = foldr f x . monotoList

    {- | Right-associative, strict in the accumulator, fold of a monofoldable. -}
    {-# INLINE monofoldl #-}
    monofoldl :: (a -> ElementOf f -> a) -> a -> f -> a
    monofoldl f x = foldl' f x . monotoList

    {- | Return true if the monofoldable has no elements. -}
    {-# INLINE mononull #-}
    mononull :: f -> Bool
    mononull = null . monotoList

    {- | The number of elements in the monofoldable. -}
    {-# INLINE monolength #-}
    monolength :: f -> Word
    monolength = fromIntegral . length . monotoList

    {- | Return True if @elem@ is an element of the monofoldable. -}
    {-# INLINE monoelem #-}
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

instance MonoFoldable LazyBytes.ByteString where
    {-# INLINE monotoList #-}
    monotoList :: LazyBytes.ByteString -> [Word8]
    monotoList = LazyBytes.unpack

    {-# INLINE monofoldr #-}
    monofoldr :: (Word8 -> a -> a) -> a -> LazyBytes.ByteString -> a
    monofoldr = LazyBytes.foldr

    {-# INLINE monofoldl #-}
    monofoldl :: (a -> Word8 -> a) -> a -> LazyBytes.ByteString -> a
    monofoldl = LazyBytes.foldl'

    {-# INLINE mononull #-}
    mononull :: LazyBytes.ByteString -> Bool
    mononull = LazyBytes.null

    {-# INLINE monolength #-}
    monolength :: LazyBytes.ByteString -> Word
    monolength = fromIntegral . LazyBytes.length

    {-# INLINE monoelem #-}
    monoelem :: Word8 -> LazyBytes.ByteString -> Bool
    monoelem = LazyBytes.elem

instance MonoFoldable ShortBytes.ShortByteString where
    {-# INLINE monotoList #-}
    monotoList :: ShortBytes.ShortByteString -> [Word8]
    monotoList = ShortBytes.unpack

    {-# INLINE monofoldr #-}
    monofoldr :: (Word8 -> a -> a) -> a -> ShortBytes.ShortByteString -> a
    monofoldr = ShortBytes.foldr

    {-# INLINE monofoldl #-}
    monofoldl :: (a -> Word8 -> a) -> a -> ShortBytes.ShortByteString -> a
    monofoldl = ShortBytes.foldl'

    {-# INLINE mononull #-}
    mononull :: ShortBytes.ShortByteString -> Bool
    mononull = ShortBytes.null

    {-# INLINE monolength #-}
    monolength :: ShortBytes.ShortByteString -> Word
    monolength = fromIntegral . ShortBytes.length

    {-# INLINE monoelem #-}
    monoelem :: Word8 -> ShortBytes.ShortByteString -> Bool
    monoelem = ShortBytes.elem

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

instance MonoFoldable LazyText.Text where
    {-# INLINE monotoList #-}
    monotoList :: LazyText.Text -> [Char]
    monotoList = LazyText.unpack

    {-# INLINE monofoldr #-}
    monofoldr :: (Char -> a -> a) -> a -> LazyText.Text -> a
    monofoldr = LazyText.foldr

    {-# INLINE monofoldl #-}
    monofoldl :: (a -> Char -> a) -> a -> LazyText.Text -> a
    monofoldl = LazyText.foldl'

    {-# INLINE mononull #-}
    mononull :: LazyText.Text -> Bool
    mononull = LazyText.null

    {-# INLINE monolength #-}
    monolength :: LazyText.Text -> Word
    monolength = fromIntegral . LazyText.length

    {-# INLINE monoelem #-}
    monoelem :: Char -> LazyText.Text -> Bool
    monoelem = LazyText.elem

instance MonoFoldable [a] where
    {-# INLINE monotoList #-}
    monotoList :: [a] -> [a]
    monotoList = id

    {-# INLINE monofoldMap #-}
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

    {-# INLINE monofoldMap #-}
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

    {-# INLINE monofoldMap #-}
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

instance MonoFoldable (StrictVector.Vector a) where
    {-# INLINE monotoList #-}
    monotoList :: StrictVector.Vector a -> [a]
    monotoList = toList

    {-# INLINE monofoldr #-}
    monofoldr :: (a -> b -> b) -> b -> StrictVector.Vector a -> b
    monofoldr = foldr

    {-# INLINE monofoldl #-}
    monofoldl :: (b -> a -> b) -> b -> StrictVector.Vector a -> b
    monofoldl = foldl'

    {-# INLINE mononull #-}
    mononull :: StrictVector.Vector a -> Bool
    mononull = null

    {-# INLINE monolength #-}
    monolength :: StrictVector.Vector a -> Word
    monolength = fromIntegral . length

    {-# INLINE monoelem #-}
    monoelem :: Eq a => a -> StrictVector.Vector a -> Bool
    monoelem = elem

instance UnboxedVector.Unbox a => MonoFoldable (UnboxedVector.Vector a) where
    {-# INLINE monotoList #-}
    monotoList :: UnboxedVector.Vector a -> [a]
    monotoList = UnboxedVector.toList

    {-# INLINE monofoldMap #-}
    monofoldMap :: Monoid m => (a -> m) -> UnboxedVector.Vector a -> m
    monofoldMap = UnboxedVector.foldMap'

    {-# INLINE monofoldr #-}
    monofoldr :: (a -> b -> b) -> b -> UnboxedVector.Vector a -> b
    monofoldr = UnboxedVector.foldr

    {-# INLINE monofoldl #-}
    monofoldl :: (b -> a -> b) -> b -> UnboxedVector.Vector a -> b
    monofoldl = UnboxedVector.foldl'

    {-# INLINE mononull #-}
    mononull :: UnboxedVector.Vector a -> Bool
    mononull = UnboxedVector.null

    {-# INLINE monolength #-}
    monolength :: UnboxedVector.Vector a -> Word
    monolength = fromIntegral . UnboxedVector.length

    {-# INLINE monoelem #-}
    monoelem :: Eq a => a -> UnboxedVector.Vector a -> Bool
    monoelem = UnboxedVector.elem

instance StorableVector.Storable a => MonoFoldable (StorableVector.Vector a) where
    {-# INLINE monotoList #-}
    monotoList :: StorableVector.Vector a -> [a]
    monotoList = StorableVector.toList

    {-# INLINE monofoldMap #-}
    monofoldMap :: Monoid m => (a -> m) -> StorableVector.Vector a -> m
    monofoldMap = StorableVector.foldMap'

    {-# INLINE monofoldr #-}
    monofoldr :: (a -> b -> b) -> b -> StorableVector.Vector a -> b
    monofoldr = StorableVector.foldr

    {-# INLINE monofoldl #-}
    monofoldl :: (b -> a -> b) -> b -> StorableVector.Vector a -> b
    monofoldl = StorableVector.foldl'

    {-# INLINE mononull #-}
    mononull :: StorableVector.Vector a -> Bool
    mononull = StorableVector.null

    {-# INLINE monolength #-}
    monolength :: StorableVector.Vector a -> Word
    monolength = fromIntegral . StorableVector.length

    {-# INLINE monoelem #-}
    monoelem :: Eq a => a -> StorableVector.Vector a -> Bool
    monoelem = StorableVector.elem
