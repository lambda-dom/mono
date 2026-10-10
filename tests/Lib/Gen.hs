{- | Some generators used in testing. -}

module Lib.Gen (
    -- * Generators for integral, primitive types.
    integral,
    
    -- * Tuple generators.
    pair,

    -- * Enumeration generators.
    enumFull,
) where

-- Imports.
-- Base.
import Data.Bits (FiniteBits)

-- Testing.
import Test.Falsify (Gen)
import Test.Falsify.Generator (inRange)
import Test.Falsify.Range (enum, uniform)


{- | Generator for bounded integral types with a finite number of bits. Biased towards zero. -}
integral :: forall a . (Integral a, Bounded a, FiniteBits a) => Gen a
integral = inRange $ uniform @a

{- | Generator for pairs. -}
pair :: Gen a -> Gen b -> Gen (a, b)
pair g h = (,) <$> g <*> h

{- | Generator for the full range of a bounded enumeration. -}
enumFull :: (Enum a, Bounded a) => Gen a
enumFull = inRange $ enum (minBound, maxBound)
