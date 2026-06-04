# Mono.

> Soundtrack: Burial at Sea
>
> -- Mono, from "Hymn of the Immortal Wind"

This package offers an alternative to [mono-traversable](https://hackage.haskell.org/package/mono-traversable).

# A. The typeclasses.

## A. 1. `MonoFunctor`.

The `MonoFunctor` typeclass is the monomorphic variant of the base `Functor`. There is an alternative, trivial reformulation of `MonoFunctor` in terms of monoid actions. The `MonoFunctor` typeclass itself is not very interesting, but it is necessary to state some of the laws.

## A. 2. `MonoFoldable`.

There are three major differences of our version of `MonoFoldable` from the version in [mono-traversable](https://hackage.haskell.org/package/mono-traversable). The first is that all partial methods have been dropped; hopefully, no justification is needed. The second is that our version is much slimmer and it is only concerned with folding; specifically, most methods added to `Foldable` on optimization grounds (`sum`, `max`, etc.) have been dropped. The third difference is not a difference in the actual code, but in the laws we require. To see the rationale, recall that `[a]` is the free monoid over `a`, that is, for every `f :: Monoid m => a -> m` there is a _unique monoid morphism_ such that,

```haskell
foldMap f . pure == f
```

where `==` means extensional equality of functions.

note(s):

  * Strictly speaking, `[a]` does not have this universal property because of the existence of infinite lists. It is an unfortunate quirk (mostly a side effect of lazyness) that proper, finite lists and infinite lists are confused in this way.

Conceptually, the `Foldable` typeclass extends to other pointed functors by dropping the uniqueness and the monoid morphism requirements for `foldMap f`. We add the monomorphic variant of `Applicative f => foldMap f . pure = f` to `MonoFoldable` in the equivalent form `monotoList . monopoint == monopoint`. Note that `MonoPointed` _cannot_ be added as a superclass because there are "fixed length" types without a `MonoPointed` instance, just as there are `Foldable` types that are not `Applicative` or even pointed, so this is merely a conditional constraint. The class `MonoPointed` is itself very simple:

```haskell
class MonoFunctor a f => MonoPointed a f where
    monopoint :: a -> f
```

_Mononaturality_ (or _equivariance_ in the monoid action reformulation) is the obvious monomorphic variant of naturality.

__Definition__: Let `s` and `t` be two monofunctors with `a ~ ElementOf s ~ ElementOf t`. A function `h :: s -> t` is _mononatural_ if for every `f :: a -> a` we have the equality:

```haskell
monomap f . h == h . monomap f
```

Since monofunctors are not fully polymorphic, laws like mononaturality are not available via free theorems and must be explicitly required. In this vein, the first law is simply that `monotoList` is mononatural. All the other `MonoFoldable` methods have a default implementation in terms of `monotoList` and it is implicitly assumed that any overriding definitions are extensionally equal to the default ones. Their laws follow from this assumption.
