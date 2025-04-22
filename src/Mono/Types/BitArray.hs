{- |
Module: Mono.Types.BitArray

The @BitArray@ newtype-wrapper type.
-}

module Mono.Types.BitArray (
    -- * Types.
    BitArray (..),

    -- ** Eliminator.
    toIntegral,

    -- * Basic functions.
    bitCount,
    bit,
    bits,
    pack,
) where

-- Imports.
-- Base.
import Data.Bits (FiniteBits (finiteBitSize), Bits ((.|.), zeroBits, complement, testBit))
import qualified Data.Bits as Bits (bit)
import Data.Foldable (Foldable (foldl'))
import Data.Ix (Ix)

-- Package.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))


{- | Constructing integral values bit by bit. -}
newtype BitArray w = BitArray w
    deriving stock (Eq, Ord, Bounded, Ix)
    deriving newtype (Show, Enum, Num, Real, Integral, Bits, FiniteBits)


-- Instances.
instance (Integral w, FiniteBits w) => MonoFunctor (BitArray w) where
    type ElementOf (BitArray w) = Bool

    {-# INLINE monomap #-}
    monomap :: (Bool -> Bool) -> BitArray w -> BitArray w
    monomap f = pack . fmap f . bits

instance (Eq w, Integral w, FiniteBits w) => MonoFoldable (BitArray w) where
    {-# INLINE monotoList #-}
    monotoList :: BitArray w -> [Bool]
    monotoList = bits

    {-# INLINE mononull #-}
    mononull :: BitArray w -> Bool
    mononull = const False

    {-# INLINE monolength #-}
    monolength :: BitArray w -> Word
    monolength = bitCount

    {-# INLINE monoelem #-}
    monoelem :: Bool -> BitArray w -> Bool
    monoelem b n = if b then n /= zeroBits else n /= complement zeroBits


{- | Elimination function for t'BitArray'.

The inverse to the constructor v'BitArray'.
-}
{-# INLINE toIntegral #-}
toIntegral :: BitArray w -> w
toIntegral (BitArray n) = n


{- | Return the number of bits in the integral type.

The actual argument is ignored by the function and only the type matters.
-}
{-# INLINE bitCount #-}
bitCount :: FiniteBits w => w -> Word
bitCount n = fromIntegral $ finiteBitSize n

{- | Return the ith bit of the integral number.

Result is undefined if @i@ is larger than the 'bitCount' of the type.
-}
{-# INLINE bit #-}
bit :: Bits w => Word -> w -> Bool
bit i = flip testBit (fromIntegral i)

{- | Return the list of bits in the integral type from lowest to highest significance. -}
{-# INLINEABLE bits #-}
bits :: FiniteBits w => w -> [Bool]
bits w = go 0
    where
        go :: Word -> [Bool]
        go !n =
            if n == bitCount w
                then []
                else bit n w : go (succ n)

{- | Pack a list of bits into an integral value, the inverse of 'bits'.

note(s)

  * Argument list is truncated to a list of 'bitCount' length.
-}
{-# INLINEABLE pack #-}
pack :: forall w . (Integral w, FiniteBits w) => [Bool] -> w
pack = foldl' (.|.) 0 . fmap shift . zip [0 .. pred $ bitCount @w 0]
    where
        shift :: (Word, Bool) -> w
        shift (n, b) = if b then Bits.bit (fromIntegral n) else zeroBits 
