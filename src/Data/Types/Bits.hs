{- |
Module: Data.Types.Bits

The @Bits@ newtype-wrapper type.
-}

module Data.Types.Bits (
    -- * Types.
    Bits (..),

    -- ** Constructors.
    bit,

    -- * Functions.
    bitNumber,
) where

-- Imports.
-- Base.
import Data.Bits (FiniteBits (..), (.|.), testBit, zeroBits)
import qualified Data.Bits as Bits (bit)
import Data.Foldable (Foldable (foldl'))

-- Package.
import Data.MonoFunctor (MonoFunctor (..))


{- | Constructing integral values bit by bit. -}
newtype Bits w = Bits w
    deriving stock (Eq, Ord, Bounded)
    deriving newtype (Show)


-- Instances.
instance FiniteBits w => MonoFunctor (Bits w) where
    type ElementOf (Bits w) = Bool

    monomap :: (Bool -> Bool) -> Bits w -> Bits w
    monomap f (Bits n) =
        Bits $
            foldl'
                (.|.)
                zeroBits
                [if f (testBit n i) then Bits.bit i else zeroBits | i <- [0 .. finiteBitSize n]]


{- | Construct a 'Bits' value with the ith bit enabled. -}
bit :: FiniteBits w => Word -> Bits w
bit = Bits . Bits.bit . fromIntegral


{- | Return the number of bits in a @'Bits' w@. -}
bitNumber :: FiniteBits w => Bits w -> Word
bitNumber (Bits n) = fromIntegral $ finiteBitSize n
