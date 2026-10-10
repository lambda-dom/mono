module Tests.Typeclasses.MonoPointed (
    -- * Tests.
    tests,
) where

-- Imports.
-- Base.
import Data.Word (Word8)

-- Libraries.
import Data.ByteString (ByteString)
import Data.Text (Text)

-- Testing.
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (testProperty)

-- Package.
import Lib.Gen (word8, ascii)
import Lib.MonoPointed (test_monopointed_laws)


{- | Test the @MonoPointed@ laws for the @ByteString@ instance. -}
test_monopointed_laws_bytestring :: TestTree
test_monopointed_laws_bytestring =
    testProperty
        "Test MonoPointed laws of ByteString instance."
        (test_monopointed_laws @Word8 @ByteString word8)

{- | Test the @MonoFunctor@ laws for the @Text@ instance. -}
test_monopointed_laws_text :: TestTree
test_monopointed_laws_text =
    testProperty
        "Test MonoPointed laws of Text instance."
        (test_monopointed_laws @Char @Text ascii)


{- | Test group for the MonoFunctor module. -}
tests :: TestTree
tests =
    testGroup
        "Tests for the Mono.Typeclasses.MonoFunctor module."
        [
            test_monopointed_laws_bytestring,
            test_monopointed_laws_text
        ]
