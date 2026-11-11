-- Imports.
-- Testing.
import Test.Tasty (defaultMain, testGroup)

-- Package.
import qualified Tests.MonoFunctor as MonoFunctor (tests)
import qualified Tests.MonoPointed as MonoPointed (tests)
import qualified Tests.MonoFoldable as MonoFoldable (tests)


-- Main test driver.
main :: IO ()
main =
    defaultMain $
        testGroup
        "Package tests."
        [MonoFunctor.tests, MonoPointed.tests, MonoFoldable.tests]
