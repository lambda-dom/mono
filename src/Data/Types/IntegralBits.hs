{- |
Module: Data.Types.IntegralBits

The @Bits@ newtype-wrapper type.
-}

module Data.Types.IntegralBits (
    -- * Types.
    IntegralBits (..),
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
newtype IntegralBits w = IntegralBits w
    deriving stock (Eq, Ord, Bounded, Ix)
    deriving newtype (Show, Enum, Num, Real, Integral, Bits, FiniteBits)


-- Instances.
instance FiniteBits w => MonoFunctor (IntegralBits w) where
    type ElementOf (IntegralBits w) = Bool

    {-# INLINE monomap #-}
    monomap :: (Bool -> Bool) -> IntegralBits w -> IntegralBits w
    monomap f (IntegralBits n) =
        IntegralBits $
            foldl'
                (.|.)
                zeroBits
                [if f (testBit n i) then bit i else zeroBits | i <- [0 .. finiteBitSize n]]

instance (Eq w, FiniteBits w) => MonoFoldable (IntegralBits w) where
    {-# INLINE monoToList #-}
    monoToList :: IntegralBits w -> [Bool]
    monoToList (IntegralBits n) = [testBit n i | i <- [0 .. finiteBitSize n]]

    {-# INLINE monoNull #-}
    monoNull :: IntegralBits w -> Bool
    monoNull (IntegralBits n) = n == zeroBits

    {-# INLINE monoLength #-}
    monoLength :: IntegralBits w -> Word
    monoLength (IntegralBits n) = fromIntegral $ finiteBitSize n
