module Test.Main where

import Control.Bind (discard)
import Data.Function (($))
import Data.Show (show)
import Data.Tuple.Nested ((/\))
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)
import Supply (split4, supplyValue, withSupply)

main :: Effect Unit
main = withSupply \s -> do
  let (v1 /\ v2 /\ v3 /\ v4 /\ _) = split4 s
  let (v5 /\ v6 /\ v7 /\ v8 /\ _) = split4 v2
  log $ show $ supplyValue v1
  log $ show $ supplyValue v8
  log $ show $ supplyValue v3
