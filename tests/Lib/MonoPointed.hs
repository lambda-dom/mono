{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Lib.MonoPointed (
    -- * Tests.
    test_monopointed_laws,
) where

-- Imports.
-- Base.
import Data.String (fromString)

-- Testing.
import Test.Falsify (Property, pattern Fn, Fun, (.$), gen, assert)
import Test.Falsify.Generator (Function, Gen, fun)

-- Package.
import Lib.MonoFunctor (isExtensionallyEqual)

-- Module to test.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))
import Mono.Typeclasses.MonoPointed (MonoPointed (..))


{- | Tests mononaturality of monopoint. -}
test_monopointed_laws
    :: forall a s . (MonoPointed a s, Function a, Show a, Eq s)
    => Gen a
    -> Property ()
test_monopointed_laws genA = do
        f <- gen genFun
        let Fn f' = f
        x    <- gen genA
        assert $ isExtensionallyEqual
            ("monopoint . " ++ show f, monopoint @a @s . f')
            ("monomap " ++ show f ++ " . monopoint", monomap f' . monopoint @a @s)
            .$ (fromString "x", x)
    where
        genFun :: Gen (Fun a a)
        genFun = fun genA
