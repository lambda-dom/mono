-- Imports.
-- Testing.
import Test.Tasty (defaultMain, testGroup)

-- Package.
-- import qualified Tests.Typeclasses.MonoFunctor as MonoFunctor (tests)
-- import qualified Tests.Typeclasses.MonoPointed as MonoPointed (tests)
-- import qualified Tests.Typeclasses.MonoFoldable as MonoFoldable (tests)


-- Main test driver.
main :: IO ()
main =
    defaultMain $
        testGroup
        "Package mono tests."
        []
