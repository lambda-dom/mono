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
import Data.Bits (FiniteBits (..), zeroBits, (.|.), testBit)
import qualified Data.Bits as Bits (bit)
import Data.Foldable (Foldable (foldl'))

-- Package.
import Data.MonoFunctor (MonoFunctor (..))


{- | Constructing integral values bit by bit. -}
newtype Bits w = Bits w
    deriving stock (Eq, Ord, Bounded)
    deriving newtype (Show)


-- Instances.
instance FiniteBits w => Semigroup (Bits w) where
    (<>) :: Bits w -> Bits w -> Bits w
    (<>) (Bits n) (Bits m) = Bits $ n .|. m

instance FiniteBits w => Monoid (Bits w) where
    mempty :: Bits w
    mempty = Bits zeroBits

instance FiniteBits w => MonoFunctor (Bits w) where
    type ElementOf (Bits w) = Bool

    monomap :: (Bool -> Bool) -> Bits w -> Bits w
    monomap f (Bits n) =
        foldl'
            (<>)
            mempty
            [if f (testBit n i) then bit (fromIntegral i) else mempty | i <- [0 .. finiteBitSize n]]


{- | Construct a 'Bits' value with the ith bit enabled. -}
bit :: FiniteBits w => Word -> Bits w
bit = Bits . Bits.bit . fromIntegral


{- | Return the number of bits in a @'Bits' w@. -}
bitNumber :: FiniteBits w => Bits w -> Word
bitNumber (Bits n) = fromIntegral $ finiteBitSize n
