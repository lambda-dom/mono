{- |
Module: Data.Types.IntegralBytes

The @IntegralBytes@ newtype-wrapper type.
-}

module Data.Types.IntegralBytes (
    -- * Types.
    IntegralBytes,

    -- ** Constructor.
    makeIntegral,

    -- ** Eliminator.
    toIntegral,

    -- * Functions on @(Integral w, FiniteBits w)@.
    byteCount,
    byte,
    bytes,
    pack,
    packReverse,
) where

-- Imports.
-- Base.
import Data.Bits (Bits (..), FiniteBits (..))
import Data.Foldable (foldl')
import Data.Ix (Ix)
import Data.Word (Word8)

-- Package.
import Data.Types.IntegralBits (bitCount)
import Data.MonoFunctor (MonoFunctor (..))
import Data.MonoFoldable (MonoFoldable (..))


{- | Constructing integral values byte by byte. -}
newtype IntegralBytes n = IntegralBytes n
    deriving stock (Eq, Ord, Bounded, Ix)
    deriving newtype (Show, Enum, Num, Real, Integral, Bits, FiniteBits)


-- Instances.
instance (Integral n, FiniteBits n) => MonoFunctor (IntegralBytes n) where
    type ElementOf (IntegralBytes n) = Word8

    monomap :: (Word8 -> Word8) -> IntegralBytes n -> IntegralBytes n
    monomap f = pack . fmap f . bytes

instance (Eq n, Integral n, FiniteBits n) => MonoFoldable (IntegralBytes n) where
    monotoList :: IntegralBytes n -> [Word8]
    monotoList = bytes

    mononull :: IntegralBytes n -> Bool
    mononull = const False

    monolength :: IntegralBytes n -> Word
    monolength = byteCount

    monoelem :: Word8 -> IntegralBytes n -> Bool
    monoelem m n = m `elem` monotoList n


{- | Construct an 'IntegralBytes' value. -}
makeIntegral :: n -> IntegralBytes n
makeIntegral = IntegralBytes

{- | Elimination function for 'IntegralBytes'.

The inverse to 'makeIntegral'.
-}
toIntegral :: IntegralBytes n -> n
toIntegral (IntegralBytes n) = n


{- | Return the number of bytes in the integral type.

The actual argument is ignored by the function and only the type matters. It is implicitely assumed
that the number of bits is a multiple of @8@.
-}
byteCount :: FiniteBits w => w -> Word
byteCount n = bitCount n `quot` 8

{- | Return the ith byte of the integral number.

Result is undefined if @i@ is larger than the 'byteCount' of the type.
-}
byte :: (Integral w, Bits w) => Word -> w -> Word8
byte i n = fromIntegral $ shiftR (shiftL 0xff j .&. n) j
    where
        j = 8 * fromIntegral i

{- | Return the list of bytes in the integral type from lowest to highest significance. -}
bytes :: (Integral w, FiniteBits w) => w -> [Word8]
bytes n = [byte i n | i <- [0 .. byteCount n]]

{- | Pack a list of bytes into an integral value, the inverse of 'bytes'.

Argument list is truncated to a list of 'byteCount' length.
-}
pack :: forall w . (Integral w, FiniteBits w) => [Word8] -> w
pack = foldl' (.|.) 0 . fmap move . zip [0 .. count] . fmap fromIntegral
    where
        count :: Word
        count = byteCount @w 0

        move :: (Word, w) -> w
        move (m, n) = shiftL n (fromIntegral m)

{- | Pack a list of bytes into an integral value in reverse order.

Argument list is truncated to a list of 'byteCount' length.
-}
packReverse :: forall w . (Integral w, FiniteBits w) => [Word8] -> w
packReverse = foldl' (.|.) 0 . fmap move . zip [count - 1, count - 2 .. 0] . fmap fromIntegral
    where
        count :: Word
        count = byteCount @w 0

        move :: (Word, w) -> w
        move (m, n) = shiftL n (fromIntegral m)
