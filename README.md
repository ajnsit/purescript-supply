# purescript-supply
A pure, splittable, value supply for Purescript

## Usage

```purescript
spago install supply
```

Then you can use the functions in `Supply` module. Example -

```purescript
main :: Effect Unit
main = withIntSupply \intSupply -> do
  let v = map (\i -> "v" <> show i) intSupply
  let (v1 /\ v2 /\ v3 /\ v4 /\ _) = split4 v
  let (v5 /\ v6 /\ v7 /\ v8 /\ _) = split4 v2
  log $ supplyValue v1
  log $ supplyValue v1
  log $ supplyValue v8
  log $ supplyValue v3
  log $ supplyValue v8
```

The supplied values are sequential integers that are lazily resolved when they are used. So this should print -

```
v0
v0
v1
v2
v1
```

## API

### Creating a new Supply

Effectful interface -
```purescript
newSupply :: forall a. a -> (a -> a) -> Effect (Supply a)
```

Or pure interface -
```purescript
withSupply :: forall r a. a -> (a -> a) -> (Supply a -> r) -> r
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

```purescript
map :: forall a b. (a -> b) -> Supply a -> Supply b
```

Also supplies have a `Comonad` instance so you can use `extend` (or `modifySupply` which is the flipped version).

```purescript
-- | Generate a new supply by systematically applying a function to an existing supply
extend :: forall a b. (Supply a -> b) -> Supply a -> Supply b
```

```purescript
-- | Generate a new supply by systematically applying a function to an existing supply
modifySupply :: forall a b. Supply a -> (Supply a -> b) -> Supply b
```

### Specialised Supplies for convenience

For convenience, functions are provided to directly construct a supply of integers.

```purescript
-- | Create a new integer supply
newIntSupply :: Effect (Supply Int)
newIntSupply = newSupply 0 (_+1)
```

```purescript
-- | Create a new integer supply and use it without effects
withIntSupply :: forall r. (Supply Int -> r) -> r
withIntSupply = withSupply 0 (_+1)
```
