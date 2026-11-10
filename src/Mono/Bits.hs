{- |
Module: Bits.Mono

Utilities for @'Integral'@ types with @'FiniteBits'@ constraints.
-}

module Mono.Bits (
    -- * Functions on types with 'FiniteBits' constraints.
    bitCount,
    bits,
    unbits,
) where

-- Imports.
-- Base.
import Data.Bits (Bits (..), FiniteBits (..))


-- $setup
-- >>> import Data.Word


{- | Return the number of bits in @'FiniteBits' a => a@.

=== __Examples:__

>>> bitCount Word8
8

>>> bitCount Word64
64
-}
{-# INLINE bitCount #-}
bitCount :: forall a -> FiniteBits a => Int
bitCount a = finiteBitSize @a zeroBits

{- | Return the list of bits in little-endian order.

=== __Examples:__

>>> bits (0x0f :: Word8)
[True,True,True,True,False,False,False,False,False]
-}
{-# INLINE bits #-}
bits :: forall a . FiniteBits a => a -> [Bool]
bits n = fmap (testBit n) [0 .. bitCount a]

{- | Pack a list of bits as an @'Integral'@.

note(s):

    * Any elements in the list beyond the 'bitCount' are discarded.

=== __Examples:__

>>> unbits [True,True,True,True,False,False,False,False,False] :: Word8
15
-}
{-# INLINE unbits #-}
unbits :: forall a . (Integral a, FiniteBits a) => [Bool] -> a
unbits xs = foldl' (.|.) 0 . fmap f . zip [0 .. bitCount a] $ xs
    where
        f :: (Int, Bool) -> a
        f (n, b) = if b then bit n else 0
