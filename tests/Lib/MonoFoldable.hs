{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Lib.MonoFoldable (
    -- * Tests.
    test_mononaturality_monotolist,
    test_monofoldable_compatibility_laws,
) where

-- Imports.
-- Base.
import Data.List (singleton)
import Data.String (fromString)

-- Testing.
import Test.Falsify (Property, Gen, Fun, pattern Fn, (.$), gen, assert)
import Test.Falsify.Generator (Function, fun)

-- Package.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))
import Mono.Typeclasses.MonoPointed (MonoPointed (..))
import Lib.MonoFunctor (isExtensionallyEqual)

-- Module to test.
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))


{- | Test mononaturality of monotoList. -}
test_mononaturality_monotolist
    :: forall a s . (MonoFoldable a s, Function a, Show a, Show s, Eq a)
    => Gen a
    -> Gen s
    -> Property ()
test_mononaturality_monotolist genA genS = do
        xs <- gen genS
        f  <- gen genFun
        let Fn f' = f
        assert $ isExtensionallyEqual
            ("monotoList . monomap " ++ show f, monotoList . monomap f')
            ("fmap " ++ show f ++ " . monotoList", fmap f' . monotoList)
            .$ (fromString "xs", xs)
    where
        genFun :: Gen (Fun a a)
        genFun = fun genA

{- | Test compatibility law. -}
test_monofoldable_compatibility
    :: forall a s . (MonoFoldable a s, MonoPointed a s, Show a, Eq a)
    => Gen a
    -> Property ()
test_monofoldable_compatibility genA = do
        x <- gen genA
        assert $ isExtensionallyEqual
            ("monotoList . monopoint", monotoList . monopoint @a @s)
            ("monopoint", singleton)
            .$ (fromString "x", x)

{- | Test mononaturality of monotoList and the compatibility law. -}
test_monofoldable_compatibility_laws
    :: forall a s . (MonoFoldable a s, MonoPointed a s, Function a, Show a, Show s, Eq a)
    => Gen a
    -> Gen s
    -> Property ()
test_monofoldable_compatibility_laws genA genS
    =  test_mononaturality_monotolist genA genS
    *> test_monofoldable_compatibility @a @s genA
