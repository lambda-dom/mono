# Mono.

> Soundtrack: Burial at Sea
>
> -- Mono, from "Hymn of the Immortal Wind"

This package offers an alternative to [mono-traversable](https://hackage.haskell.org/package/mono-traversable). To understand the rationale, consider the following code:

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

This code will not compile and GHC will complain that the instances overlap. You could change `MonoCompose f a` to `MonoCompose a f` but then this problem, or a problem related to putting the type variable `a` in the first slot, would resurphace elsewhere. Other combinations of multiparameter typeclasses, possibly with functional dependencies, have these or their own problems; all in all, the upshot is that something has got to go. We follow [mono-traversable](https://hackage.haskell.org/package/mono-traversable) and drop the instance `Functor f => MonoFunctor (f a)`. In other words, the aim of this package is _not_ to unify monomorphic and polymorphic code, but simply to provide the monomorphic versions of the relevant typeclasses to make use of the same patterns. It is still useful to provide `MonoFunctor` instances for specific functors `f`; given the intended uses of the library, the cut-off point is functors that are also `Foldable`.

# A. The typeclasses.

## A. 1. `MonoFunctor`.

The `MonoFunctor` typeclass is the monomorphic variant of the base `Functor`. There is an alternative, trivial reformulation in terms of monoid actions (see [monoid-action](https://github.com/lambda-dom/monoid-action)), but it offers nothing special. The `MonoFunctor` typeclass itself is not terribly interesting, but it is necessary to state some of the laws.

## A. 2. `MonoFoldable`.

There are three major differences of our version of `MonoFoldable` from the version in [mono-traversable](https://hackage.haskell.org/package/mono-traversable). The first is that all partial methods have been dropped; hopefully, no further justification is needed. The second is that our version is much slimmer and it is only concerned with folding; specifically, most methods added to `Foldable` on optimization grounds (`sum`, `max`, etc.) have been dropped. The third difference is not a difference in the actual code, but in the laws we require. To see the justification, recall that `[a]` is the free monoid over `a`, that is, for every `f :: Monoid m => a -> m` there is a _unique monoid morphism_ such that,

```haskell
foldMap f . pure == f
```

where `==` means extensional equality of functions.

note(s):

  * Strictly speaking, `[a]` does not have this universal property because of the existence of infinite lists. It is an unfortunate quirk (mostly a side effect of lazyness, pun intended) that proper, finite lists and infinite lists are confused in this way.

Conceptually, the `Foldable` typeclass extends to other pointed functors by dropping the uniqueness and the monoid morphism requirements for `foldMap f`. We add the monomorphic variant of `Applicative f => foldMap f . pure = f` to `MonoFoldable` in the equivalent form `monotoList . monopoint == monopoint`. Note that `MonoPointed` _cannot_ be added as a superclass because there are "fixed length" types like `BitArray` without a `MonoPointed` instance, just as there are `Foldable` that are not `Applicative` or even pointed. The class `MonoPointed` is itself very simple:

```haskell
class MonoFunctor f => MonoPointed f where
    monopoint :: ElementOf f -> f
```

_Mononaturality_ (or _equivariance_ in the monoid action reformulation) is the obvious monomorphic variant of naturality.

__Definition__: Let `s` and `t` be two monofunctors with `a ~ ElementOf s ~ ElementOf t`. A function `h :: s -> t` is _mononatural_ if for every `f :: a -> a` we have the equality:

```haskell
monomap f . h == h . monomap f
```

Since monofunctors are not fully polymorphic, laws like mononaturality are not available via free theorems and must be explicitly required. In this vein, the first law is simply that `monotoList` is mononatural. All the other `MonoFoldable` methods have a default implementation in terms of `monotoList` and it is implicitly assumed that any overriding definitions are extensionally equal to the default ones.
