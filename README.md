# purescript-supply
A splittable value supply for Purescript

## Usage

```purescript
spago install supply
```

Then you can use the functions in `Supply` module. Example -

```purescript
main :: Effect Unit
main = withSupply \s -> do
  let (v1 /\ v2 /\ v3 /\ v4 /\ _) = split4 s
  let (v5 /\ v6 /\ v7 /\ v8 /\ _) = split4 v2
  log $ show $ supplyValue v1
  log $ show $ supplyValue v8
  log $ show $ supplyValue v3
```

The supplied values are sequential integers that are lazily resolved when they are used. So this should print -

```
0
1
2
```

## API

### Creating a new Supply

Effectful interface -
```purescript
newSupply :: forall a. a -> (a -> a) -> Effect (Supply a)
```

Or pure interface -
```purescript
withSupply :: forall r. (Supply Int -> r) -> r
```

### Getting the current value from a supply

```purescript
supplyValue    :: forall a. Supply a -> a
```

Each supply can only provide a single value. Repeatedly calling `supplyValue` on the same supply will give the same result.

### Splitting Supplies

To get multiple values, supplies can be split any number of times.

```purescript
-- | Split a supply into two different supplies.
-- The resulting supplies are different from the input supply.
split2         :: forall a. Supply a -> Tuple (Supply a) (Supply a)

-- | Split a supply into three different supplies.
split3         :: forall a. Supply a -> Tuple3 (Supply a) (Supply a) (Supply a)

-- | Split a supply into four different supplies.
split4         :: forall a. Supply a -> Tuple4 (Supply a) (Supply a) (Supply a) (Supply a)
```

### Modifying supplies

`Supply` has a `Functor` instance.

Also supplies have a comonad instance so you can use `modifySupply` a.k.a. `cobind`.

```purescript
-- | Generate a new supply by systematically applying a function to an existing supply
modifySupply :: forall a b. Supply a -> (Supply a -> b) -> Supply b
```

### Misc

This repo includes an implemntation of `unsafeInterleaveEffect` -

```purescript
-- | unsafeInterleaveEffect allows an Effect to be deferred lazily.
--   When passed a value of type Effect a, the Effect *should* only be performed when the value of the a is demanded.
-- TODO: Put this in purescript-effect
unsafeInterleaveEffect :: forall a. Effect a -> Effect {val :: a}
```
