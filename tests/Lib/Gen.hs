{- | Some generators used in testing. -}

module Lib.Gen (
    -- * Generators for integral, primitive types.
    integral,

    -- ** Specializations.
    word8,

    -- * List-like generators.
    byteString,
) where

-- Imports.
-- Base.
import Data.Bits (FiniteBits)
import Data.Word (Word8)

-- Libraries.
import qualified Data.ByteString as Bytes (ByteString, pack)

-- Testing.
import Test.Falsify (Gen)
import Test.Falsify.Generator (inRange, list)
import Test.Falsify.Range (uniform, inclusive)


{- | Generator for bounded integral types with a finite number of bits. Biased towards zero. -}
integral :: forall a . (Integral a, Bounded a, FiniteBits a) => Gen a
integral = inRange $ uniform @a

{- | Specialization of 'integral' to 'Word8'. -}
word8 :: Gen Word8
word8 = integral

{- | Generate a (strict) 'ByteString' up to length @n@ shrinking towards 0. -}
byteString :: Word -> Gen Bytes.ByteString
byteString n = Bytes.pack <$> list (inclusive (0, n)) word8
