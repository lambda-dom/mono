{- |
Module: Data.Types.IntegralBits

The @IntegralBits@ newtype-wrapper type.
-}

module Data.Types.IntegralBits (
    -- * Types.
    IntegralBits,
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
instance FiniteBits n => MonoFunctor (IntegralBits n) where
    type ElementOf (IntegralBits n) = Bool

    monomap :: (Bool -> Bool) -> IntegralBits n -> IntegralBits n
    monomap f n =
        foldl'
            (.|.)
            zeroBits
            [if f (testBit n i) then bit i else zeroBits | i <- [0 .. finiteBitSize n]]

instance (Eq n, FiniteBits n) => MonoFoldable (IntegralBits n) where
    monotoList :: IntegralBits n -> [Bool]
    monotoList n = [testBit n i | i <- [0 .. finiteBitSize n]]

    mononull :: IntegralBits n -> Bool
    mononull = const False

    monolength :: IntegralBits n -> Word
    monolength = fromIntegral . finiteBitSize

    monoelem :: Bool -> IntegralBits n -> Bool
    monoelem b n = if b then n /= zeroBits else n /= complement zeroBits
