{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}

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
        Fn f <- gen genFun
        x    <- gen genA
        assert $ isExtensionallyEqual
            ("Monomap post-composition", monop . f)
            ("Monomap pre-composition", monomap f . monop)
            .$ (fromString "x", x)
    where
        genFun :: Gen (Fun a a)
        genFun = fun genA

        monop :: a -> s
        monop = monopoint
