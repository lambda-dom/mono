{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}

module Lib.MonoFunctor (
    -- * Predicates.
    isExtensionallyEqual,

    -- * Properties.
    -- ** Extensional equality of functions.
    test_extensional_equality,

    -- * 'MonoFunctor' typeclass laws.
    test_monofunctor_laws,

    -- * Mononaturality.
    test_mononaturality,
) where

-- Imports.
-- Base.
import Data.String (fromString)

-- Testing.
import Test.Falsify (Property, Gen, Predicate, Fun, pattern Fn, (.$), assert, gen)
import Test.Falsify.Generator (Function, fun)
import Test.Falsify.Predicate (satisfies)

-- Module to test.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))


{- | Predicate for functional, extensional equality of functions. -}
isExtensionallyEqual
    :: Eq b
    => (String, a -> b)
    -> (String, a -> b)
    -> Predicate '[a]
isExtensionallyEqual (xs, f) (ys, g) =
    satisfies (fromString $ "Predicate " ++ xs ++ " == " ++ ys, \ x -> f x == g x)


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
        ("monomap id", monomap id)
        ("id", id)

{- | Test 'MonoFunctor' composition law. -}
test_monofunctor_composition
    :: forall s a . (MonoFunctor a s, Show s, Eq s, Show a, Function a)
    => Gen a
    -> Gen s
    -> Property ()
test_monofunctor_composition genA genS = do
        f <- gen genFun
        g <- gen genFun
        let
            Fn f' = f
            Fn g' = g
        xs <- gen genS
        assert $ isExtensionallyEqual
            ("monomap (" ++ show f ++ " . " ++ show g ++ ")", monomap (f' . g'))
            ("monomap (" ++ show f ++ ") . monomap (" ++ show g ++ ")", monomap f' . monomap g')
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


{- | Test mononaturality of a function. -}
test_mononaturality
    :: forall a s t . (MonoFunctor a s, MonoFunctor a t, Function a, Show a, Show s, Eq t)
    => Gen a
    -> Gen s
    -> (String, s -> t)
    -> Property ()
test_mononaturality genA genS (xs, h) = do
        f <- gen genFun
        let Fn f' = f
        ys   <- gen genS
        assert $ isExtensionallyEqual
            (xs ++ " . monomap " ++ show f, h . monomap f')
            ("monomap " ++ show f ++ " . " ++ xs, monomap f' . h)
            .$ (fromString "ys", ys)
    where
        genFun :: Gen (Fun a a)
        genFun = fun genA
