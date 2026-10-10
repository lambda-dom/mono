{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}

module Lib.MonoFunctor (
    -- * Properties.
    -- ** Extensional equality of functions.
    test_extensional_equality,

    -- * Typeclass laws.
    test_monofunctor_laws,
) where

-- Imports.
-- Base.
import Data.String (fromString)

-- Testing.
import Test.Falsify (Property, Gen, Predicate, Fun, pattern Fn, (.$), assert, gen)
import Test.Falsify.Generator (Function, fun)
import Test.Falsify.Predicate (prettyExpr, unary)

-- Module to test.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))


{- | Predicate for functional, extensional equality of functions. -}
isExtensionallyEqual
    :: Eq b
    => (String, a -> b)
    -> (String, a -> b)
    -> Predicate '[a]
isExtensionallyEqual (xs, f) (ys, g) =
    unary
        (\ x -> f x == g x)
        (\ e -> let zs = " applied to " ++ prettyExpr e in xs ++ zs ++ " not equal to " ++ ys ++ zs)


{- | Test extensional equality of two functions. -}
test_extensional_equality
    :: (Show a, Eq b)
    => (String, a -> b)
    -> (String, a -> b)
    -> Gen a
    -> Property ()
test_extensional_equality p q genArg = do
        x <- gen genArg
        assert $ isExtensionallyEqual p q .$ (fromString "x", x)


{- | Test 'MonoFunctor' identity law. -}
test_monofunctor_identity
    :: (MonoFunctor a s, Show s, Eq s)
    => Gen s
    -> Property ()
test_monofunctor_identity =
    test_extensional_equality
        ("Monomap of the identity", monomap id)
        ("Identity", id)

{- | Test 'MonoFunctor' composition law. -}
test_monofunctor_composition
    :: forall s a . (MonoFunctor a s, Show s, Eq s, Show a, Function a)
    => Gen a
    -> Gen s
    -> Property ()
test_monofunctor_composition genA genS = do
        Fn f <- gen genFun
        Fn g <- gen genFun
        xs <- gen genS
        assert $ isExtensionallyEqual
            ("Monomap composition", monomap (f . g))
            ("Composition of monomaps", monomap f . monomap g)
            .$ (fromString "xs", xs)
    where
        genFun :: Gen (Fun a a)
        genFun = fun genA

test_monofunctor_laws
    :: (MonoFunctor a s, Show s, Eq s, Show a, Function a)
    => Gen a
    -> Gen s
    -> Property ()
test_monofunctor_laws genA genS
    =  test_monofunctor_identity genS
    *> test_monofunctor_composition genA genS
