{- | Some generators used in testing. -}

module Lib.Gen (
    -- * Generators for integral, primitive types.
    integral,

    -- ** Specializations.
    word8,

    -- * Other primitive generators.
    char,
    safeChar,
    ascii,

    -- * List-like generators.
    byteString,

    -- ** Specializations.
    text,
    asciiText,
) where

-- Imports.
-- Base.
import Data.Bits (FiniteBits)
import Data.Char (chr)
import Data.Word (Word8)

-- Libraries.
import qualified Data.ByteString as Bytes (ByteString, pack)
import qualified Data.Text as Text (Text, pack)
import qualified Data.Text.Internal as Internal (safe)

-- Testing.
import Test.Falsify (Gen)
import Test.Falsify.Generator (inRange, list, int)
import Test.Falsify.Range (uniform, inclusive, enum)


{- | Generator for bounded integral types with a finite number of bits. Biased towards zero. -}
integral :: forall a . (Integral a, Bounded a, FiniteBits a) => Gen a
integral = inRange $ uniform @a

{- | Specialization of 'integral' to 'Word8'. -}
word8 :: Gen Word8
word8 = integral

{- | Generator for 'Char'. -}
char :: Gen Char
char = inRange $ enum (maxBound, minBound)

{- | Generator for 'Char'.

Performs replacement on invalid scalar values using the 'safe' function from 'Data.Text.Internal'.
This is required when using the text package as many functions perform such a replacement.
-}
safeChar :: Gen Char
safeChar = Internal.safe <$> char

{- | Generator for an ascii 'Char' except the null character. -}
ascii :: Gen Char
ascii = fmap chr . int $ inclusive (1, 127)

{- | Generate a (strict) 'ByteString' up to length @n@ shrinking towards 0. -}
byteString :: Word -> Gen Bytes.ByteString
byteString n = Bytes.pack <$> list (inclusive (0, n)) word8

{- | Generate a (strict) 'Text' up to length @n@ shrinking towards 0. -}
text :: Word -> Gen Text.Text
text n = Text.pack <$> list (inclusive (0, n)) char

{- | Generate a (strict) 'Text' up to length @n@ consisting of ascii characters minus the null one. -}
asciiText :: Word -> Gen Text.Text
asciiText n = Text.pack <$> list (inclusive (0, n)) ascii
