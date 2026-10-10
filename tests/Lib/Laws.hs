{-# LANGUAGE DataKinds #-}

module Lib.Laws (
    -- * Properties.
    -- ** Extensional equality of functions.
    test_extensional_equality,
    test_extensional_equality_binary,
) where

-- Imports.
-- Base.
import Data.String (fromString)

-- Testing.
import Test.Falsify (Property, Gen, Predicate, (.$), assert, gen)
import Test.Falsify.Predicate (prettyExpr, unary, binary)


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

{- | Predicate for functional, extensional equality of binary functions. -}
isExtensionallyEqualBinary
    :: Eq c
    => (String, a -> b -> c)
    -> (String, a -> b -> c)
    -> Predicate '[a, b]
isExtensionallyEqualBinary (xs, f) (ys, g) =
    binary
        (\ x y -> f x y == g x y)
        (\ e1 e2 ->
            let
                zs = " applied to " ++ prettyExpr e1 ++ " and " ++ prettyExpr e2
            in
                xs ++ zs ++ " not equal to " ++ ys ++ zs)
        

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

{- | Test extensional equality of two binary functions. -}
test_extensional_equality_binary
    :: (Show a, Show b, Eq c)
    => (String, a -> b -> c)
    -> (String, a -> b -> c)
    -> Gen a
    -> Gen b
    -> Property ()
test_extensional_equality_binary p q gen1 gen2 = do
        x <- gen gen1
        y <- gen gen2
        assert $ isExtensionallyEqualBinary p q .$ (fromString "x", x) .$ (fromString "y", y)
