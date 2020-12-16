module Test.Main where

import Control.Bind (discard)
import Data.Function (($))
import Data.Functor (map)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Tuple.Nested ((/\))
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)
import Supply (split4, supplyValue, withIntSupply)

main :: Effect Unit
main = withIntSupply \intSupply -> do
  let v = map (\i -> "v" <> show i) intSupply
  let (v1 /\ v2 /\ v3 /\ v4 /\ _) = split4 v
  let (v5 /\ v6 /\ v7 /\ v8 /\ _) = split4 v2
  log $ supplyValue v1 -- v0
  log $ supplyValue v1 -- v0
  log $ supplyValue v8 -- v1
  log $ supplyValue v3 -- v2
  log $ supplyValue v8 -- v1
