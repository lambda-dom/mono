module Tests.Typeclasses.MonoFunctor (
    -- * Tests.
    tests,
) where

-- Imports.
-- Testing.
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (testProperty)

-- Package.
import Lib.Gen (byteString, word8, asciiText, ascii)
import Lib.MonoFunctor (test_monofunctor_laws)


{- | Test the @MonoFunctor@ laws for the @ByteString@ instance. -}
test_monofunctor_laws_bytestring :: TestTree
test_monofunctor_laws_bytestring =
    testProperty
        "Test MonoFunctor laws of ByteString instance."
        (test_monofunctor_laws word8 (byteString 20))

{- | Test the @MonoFunctor@ laws for the @Text@ instance. -}
test_monofunctor_laws_text :: TestTree
test_monofunctor_laws_text =
    testProperty
        "Test MonoFunctor laws of Text instance."
        (test_monofunctor_laws ascii (asciiText 10))


{- | Test group for optics in the Bits module. -}
tests :: TestTree
tests =
    testGroup
        "Tests for the Mono.Typeclasses.MonoFunctor module."
        [
            test_monofunctor_laws_bytestring,
            test_monofunctor_laws_text
        ]
