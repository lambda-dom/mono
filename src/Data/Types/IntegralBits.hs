{- |
Module: Data.Types.IntegralBits

The @IntegralBits@ newtype-wrapper type.
-}

module Data.Types.IntegralBits (
    -- * Types.
    IntegralBits,

    -- * Basic functions.
    bitCount,
    fromBit,
    bits,
    pack,
) where

-- Imports.
-- Base.
import Data.Bits (FiniteBits (..), Bits (..))
import Data.Foldable (Foldable (foldl'))
import Data.Ix (Ix)

-- Package.
import Data.MonoFunctor (MonoFunctor (..))
import Data.MonoFoldable (MonoFoldable (..))


{- | Constructing integral values bit by bit. -}
newtype IntegralBits n = IntegralBits n
    deriving stock (Eq, Ord, Bounded, Ix)
    deriving newtype (Show, Enum, Num, Real, Integral, Bits, FiniteBits)


-- Instances.
instance (Integral n, FiniteBits n) => MonoFunctor (IntegralBits n) where
    type ElementOf (IntegralBits n) = Bool

    monomap :: (Bool -> Bool) -> IntegralBits n -> IntegralBits n
    monomap f = pack . fmap f . bits

instance (Eq n, Integral n, FiniteBits n) => MonoFoldable (IntegralBits n) where
    monotoList :: IntegralBits n -> [Bool]
    monotoList = bits

    mononull :: IntegralBits n -> Bool
    mononull = const False

    monolength :: IntegralBits n -> Word
    monolength = bitCount

    monoelem :: Bool -> IntegralBits n -> Bool
    monoelem b n = if b then n /= zeroBits else n /= complement zeroBits


{- | Return the number of bits in the integral type.

The actual argument is ignored by the function and only the type matters.
-}
{-# INLINE bitCount #-}
bitCount :: FiniteBits w => w -> Word
bitCount n = fromIntegral $ finiteBitSize n

{- | Return the ith bit of the integral number.

Result is undefined if @i@ is larger than the 'bitCount' of the type.
-}
{-# INLINE isEnabled #-}
isEnabled :: Bits w => Word -> w -> Bool
isEnabled i = flip testBit (fromIntegral i)

{- | Return the list of bits in the integral type from lowest to highest significance. -}
bits :: FiniteBits w => w -> [Bool]
bits n = [isEnabled i n | i <- [0 .. bitCount n]]

{- | Make an integral value with ith bit @b@. -}
fromBit :: FiniteBits w => Word -> Bool -> w 
fromBit i b = if b then bit (fromIntegral i) else zeroBits

{- | Pack a list of bytes into an integral value.

Result is undefined if the length of the list is larger than the 'bitCount' of the type.
-}
pack :: forall w . (Integral w, Bits w) => [Bool] -> w
pack = foldl' (.|.) 0 . fmap move . zip [0 ..] . fmap convert
    where
        convert :: Bool -> w
        convert False = 0
        convert True  = 1

        move :: (Word, w) -> w
        move (n, m) = shiftR m (fromIntegral n)
