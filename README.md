# Mono.

> Soundtrack: Burial at Sea
>
> -- Mono, from "Hymn of the Immortal Wind"

This package offers an alternative [mono-traversable](https://hackage.haskell.org/package/mono-traversable). To understand the rationale consider the following code:

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

This code will not compile and GHC will complain that the instances overlap. You could change `MonoCompose f a` to `MonoCompose a f` but then the problem would resurphace elsewhere, say with the newtype-wrapper `BitArray w`. Other combinations of multiparameter typeclasses, possibly with functional dependencies, have this or their own problems; all in all, the upshot is that something has got to go. We follow [mono-traversable](https://hackage.haskell.org/package/mono-traversable) and drop the instance `Functor f => MonoFunctor (f a)`. In other words, the aim of this package is _not_ to unify monomorphic and polymorphic code, but simply to provide the monomorphic versions of the relevant typeclasses. It is still useful to provide `MonoFunctor` instances for specific functors `f`; given the intended use the library, the cut-off point is functors that are also `Foldable` and have an `uncons` operation.

# A. The typeclasses.

## A. 1. `MonoFunctor`.

The `MonoFunctor` typeclass is the monomorphic variant of the base `Functor`. There is an alternative, trivial reformulation in terms of monoid actions (see [monoid-action](https://github.com/lambda-dom/monoid-action)), but it offers nothing special. The `MonoFunctor` typeclass itself is not terribly interesting, but it is necessary to state some of the laws.

## A. 2. `MonoFoldable`.

There are three major differences of our version of `MonoFoldable` from the version in [mono-traversable](https://hackage.haskell.org/package/mono-traversable). The first is that all partial methods have been dropped; hopefully, no justification is needed. The second is that our version is much slimmer and it is only concerned with folding; specifically, most methods added to `Foldable` on optimization grounds (`sum`, `max`, etc.) have been dropped. The third difference is that it actually has an extra method, as implied by the `MonoPointed` superclass. To see the justification, recall that `[a]` is the free monoid over `a`, that is, for every `f :: Monoid m => a -> m` there is a _unique monoid morphism_ such that,

```haskell
foldMap f . pure == f
```

where `==` means extensional equality of functions.

note(s):

  * Strictly speaking, `[a]` does not have this universal property because of the existence of infinite lists. It is an unfortunate quirk (mostly a side effect of lazyness, pun intended) that proper, finite lists and infinite lists are confused in this way.

Conceptually, the `Foldable` typeclass extends this to other types by dropping the uniqueness and the monoid morphism requirements for `foldMap f`. The equation `foldMap f . pure == f` is missing from `Foldable`, so we add it to `MonoFoldable` in the equivalent form `toList . monopoint == monopoint`. The class `MonoPointed` is itself very simple:

```haskell
class MonoFunctor f => MonoPointed f where
    {- | Embed an element in the 'MonoFunctor'.

    The method 'monopoint' must be mononatural.
    -}
    monopoint :: ElementOf f -> f
```

_Mononaturality_ (or _equivariance_ in the monoid action reformulation) is the obvious monomorphic variant of naturality.

__Definition__: Let `s` and `t` be two monofunctors with `a ~ 'ElementOf' s ~ 'ElementOf' t`. A
function `h :: s -> t` is _mononatural_ if for every `f :: a -> a` we have the
equality:

```haskell
monomap f . h == h . monomap f
```
