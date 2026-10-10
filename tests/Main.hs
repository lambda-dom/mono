-- Imports.
-- Testing.
import Test.Tasty (defaultMain, testGroup)

-- Package.
import qualified Tests.Typeclasses.MonoFunctor as MonoFunctor (tests)


-- Main test driver.
main :: IO ()
main =
    defaultMain $
        testGroup
        "Package tests."
        [MonoFunctor.tests]
