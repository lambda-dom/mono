{- |
Module: Mono.Types.IntegralBytes

The @IntegralBytes@ newtype-wrapper type.
-}

module Mono.Types.ByteArray (
    -- * Types.
    ByteArray (..),

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
import Data.Bits (Bits ((.&.), (.|.), shiftL, shiftR), FiniteBits)
import Data.Foldable (foldl')
import Data.Ix (Ix)
import Data.Word (Word8)

-- Package.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))
import Mono.Types.BitArray (bitCount)


{- | Constructing integral values byte by byte. -}
newtype ByteArray w = ByteArray w
    deriving stock (Eq, Ord, Bounded, Ix)
    deriving newtype (Show, Enum, Num, Real, Integral, Bits, FiniteBits)


-- Instances.
instance (Integral w, FiniteBits w) => MonoFunctor (ByteArray w) where
    type ElementOf (ByteArray w) = Word8

    monomap :: (Word8 -> Word8) -> ByteArray w -> ByteArray w
    monomap f = pack . fmap f . bytes

instance (Eq w, Integral w, FiniteBits w) => MonoFoldable (ByteArray w) where
    monotoList :: ByteArray w -> [Word8]
    monotoList = bytes

    mononull :: ByteArray w -> Bool
    mononull = const False

    monolength :: ByteArray w -> Word
    monolength = byteCount

    monoelem :: Word8 -> ByteArray w -> Bool
    monoelem m n = m `elem` monotoList n


{- | Elimination function for t'ByteArray'.

The inverse to the v'ByteArray' constructor.
-}
toIntegral :: ByteArray w -> w
toIntegral (ByteArray n) = n


{- | Return the number of bytes in the integral type.

The actual argument is ignored by the function and only the type matters. It is implicitely assumed
that the number of bits is a multiple of @8@.
-}
{-# INLINE byteCount #-}
byteCount :: FiniteBits w => w -> Word
byteCount n = bitCount n `quot` 8

{- | Return the ith byte of the integral number.

Result is undefined if @i@ is larger than the 'byteCount' of the type.
-}
{-# INLINE byte #-}
byte :: (Integral w, Bits w) => Word -> w -> Word8
byte i n = fromIntegral $ shiftR (shiftL 0xff j .&. n) j
    where
        j = 8 * fromIntegral i

{- | Return the list of bytes from lowest to highest significance. -}
{-# INLINEABLE bytes #-}
bytes :: (Integral w, FiniteBits w) => w -> [Word8]
bytes w = go 0
    where
        go :: Word -> [Word8]
        go !n =
            if n == byteCount w
                then []
                else byte n w : go (succ n)

{- | Shift an integral @n@ bytes left. -}
{-# INLINE shiftByteL #-}
shiftByteL :: Bits w => Word -> w -> w
shiftByteL m n = shiftL n ( 8 * fromIntegral m)

{- | Pack a list of bytes into an integral value, the inverse of 'bytes'.

note(s):

  * Argument list is truncated to a list of 'byteCount' length.
-}
{-# INLINEABLE pack #-}
pack :: forall w . (Integral w, FiniteBits w) => [Word8] -> w
pack
    = foldl' (.|.) 0
    . fmap (uncurry shiftByteL)
    . zip [0 .. pred $ byteCount @w 0]
    . fmap fromIntegral

{- | Pack a list of bytes into an integral value in reverse order.

Equivalent to, but more efficient than, @'pack' . reverse@.

note(s)

  * Argument list is truncated to a list of 'byteCount' length.
-}
{-# INLINEABLE packReverse #-}
packReverse :: forall w . (Integral w, FiniteBits w) => [Word8] -> w
packReverse
        = foldl' (.|.) 0
        . fmap (uncurry shiftByteL)
        . zip [count - 1, count - 2 .. 0]
        . fmap fromIntegral
    where
        count :: Word
        count = byteCount @w 0
