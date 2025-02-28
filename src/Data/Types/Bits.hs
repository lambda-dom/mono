{- |
Module: Data.Types.Bits

The @Bits@ newtype-wrapper type.
-}

module Data.Types.Bits (
    -- * Types.
    Bits (..),
) where

-- Imports.
-- Base.
import Data.Bits (FiniteBits (..), (.|.), testBit, zeroBits)
import qualified Data.Bits as Bits (bit)
import Data.Foldable (Foldable (foldl'))
import Data.Ix (Ix)

-- Package.
import Data.MonoFunctor (MonoFunctor (..))
import Data.MonoFoldable (MonoFoldable (..))


{- | Constructing integral values bit by bit. -}
newtype Bits w = Bits w
    deriving stock (Eq, Ord, Bounded, Ix)
    deriving newtype (Show)


-- Instances.
instance FiniteBits w => MonoFunctor (Bits w) where
    type ElementOf (Bits w) = Bool

    {-# INLINE monomap #-}
    monomap :: (Bool -> Bool) -> Bits w -> Bits w
    monomap f (Bits n) =
        Bits $
            foldl'
                (.|.)
                zeroBits
                [if f (testBit n i) then Bits.bit i else zeroBits | i <- [0 .. finiteBitSize n]]

instance (Eq w, FiniteBits w) => MonoFoldable (Bits w) where
    {-# INLINE monoToList #-}
    monoToList :: Bits w -> [Bool]
    monoToList (Bits n) = [testBit n i | i <- [0 .. finiteBitSize n]]

    {-# INLINE monoNull #-}
    monoNull :: Bits w -> Bool
    monoNull (Bits n) = n == zeroBits

    {-# INLINE monoLength #-}
    monoLength :: Bits w -> Word
    monoLength (Bits n) = fromIntegral $ finiteBitSize n
