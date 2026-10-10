module Tests.Typeclasses.MonoFunctor (
    -- * Tests.
    tests,
) where

-- Imports.
-- Testing.
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (testProperty)

-- Package.
import Lib.Gen (byteString, word8)
import Lib.MonoFunctor (test_monofunctor_laws)

-- Module to test.


{- | Test the @MonoFunctor@ laws for the ByteString instance. -}
test_monofunctor_laws_bytestring :: TestTree
test_monofunctor_laws_bytestring =
    testProperty
        "Test MonoFunctor laws of ByteString instance."
        (test_monofunctor_laws word8 (byteString 20))

{- | Test group for optics in the Bits module. -}
tests :: TestTree
tests =
    testGroup
        "Tests for the Mono.Typeclasses.MonoFunctor module."
        [test_monofunctor_laws_bytestring]
