{- |
Module: Mono.Typeclasses.MonoPointed

The @MonoPointed@ typeclass.
-}

module Mono.Typeclasses.MonoPointed (
    -- * Typeclasses.
    MonoPointed (..),
) where

-- Imports.
-- Package.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (ElementOf))


{- | The @MonoPointed@ typeclass.

There is one law that the typeclass must satisfy:

__Mononaturality__: @'monopoint' :: f -> ['ElementOf' f]@ is mononatural.
-}
class MonoFunctor f => MonoPointed f where
    {- | Embed an element in the 'MonoFunctor'. -}
    monopoint :: ElementOf f -> f
