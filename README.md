# Mono.

> Soundtrack: Burial at Sea
>
> -- Mono, from "Hymn of the Immortal Wind"

This package offers an alternative [mono-traversable](https://hackage.haskell.org/package/mono-traversable). To understand the rational consider the following code:

```haskell
{- | The typeclass for monofunctors, the monomorphic version of 'Functor'. -}
class MonoFunctor f where
    {-# MINIMAL monomap #-}

    {- | The type of the elements of the monofunctor. -}
    type ElementOf f :: Type

    {- | Map over a monofunctor. -}
    monomap :: (ElementOf f -> ElementOf f) -> f -> f

instance Functor f => MonoFunctor (f a) where
    type ElementOf (f a) = a

    monomap :: (a -> a) -> f a -> f a
    monomap = fmap

{- | Newtype-wrapper around @f a@ for a functor @f@ and a monofunctor @a@. -}
newtype MonoCompose f a = MonoCompose (f a)
    deriving newtype (Eq, Show)

-- Instances.
instance (Functor f, MonoFunctor a) => MonoFunctor (MonoCompose f a) where
    type ElementOf (MonoCompose f a) = ElementOf a

    monomap :: (ElementOf a -> ElementOf a) -> MonoCompose f a -> MonoCompose f a
    monomap f (MonoCompose xs) = MonoCompose (fmap (monomap f) xs)
```

This code will not compile and GHC will complain that the instances overlap. You could change `MonoCompose f a` to `MonoCompose a f` but then the problem would resurphace elsewhere, say with the newtype-wrapper `IntegralBits w`. Other combinations of multiparameter typeclasses, possibly with functional dependencies, have this or their own problems; all in all, the upshot is that something has got to go. We follow [mono-traversable](https://hackage.haskell.org/package/mono-traversable) and drop the instance `Functor f => MonoFunctor (f a)`. In other words, the aim of this package is _not_ to unify monomorphic and polymorphic code, but simply to provide the momorphic versions of the relevant typeclasses. It is still useful to provide `MonoFunctor` instances for specific functors `f`; our cut-off point is functors that are also `Foldable` and have an `uncons` operation.
