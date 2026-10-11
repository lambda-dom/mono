module Tests.Typeclasses.MonoFoldable (
    -- * Tests.
    tests,
) where

-- Imports.
-- Testing.
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (testProperty)

-- Package.
import Lib.Gen (word8, byteString, ascii, asciiText)
import Lib.MonoFoldable (test_monofoldable_compatibility_laws)


{- | Test the @MonoFunctor@ laws for the @ByteString@ instance. -}
test_monofoldable_laws_bytestring :: TestTree
test_monofoldable_laws_bytestring =
    testProperty
        "Test MonoFoldable laws of ByteString instance."
        (test_monofoldable_compatibility_laws word8 (byteString 20))

{- | Test the @MonoFoldable@ laws for the @Text@ instance. -}
test_monofoldable_laws_text :: TestTree
test_monofoldable_laws_text =
    testProperty
        "Test MonoFoldable laws of Text instance."
        (test_monofoldable_compatibility_laws ascii (asciiText 10))


{- | Test group for the MonoFunctor module. -}
tests :: TestTree
tests =
    testGroup
        "Tests for the Mono.Typeclasses.MonoFunctor module."
        [
            test_monofoldable_laws_bytestring,
            test_monofoldable_laws_text
        ]
