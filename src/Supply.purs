module Supply where

import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Data.CommutativeRing ((+))
import Data.Function (($))
import Data.Functor (class Functor)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (Tuple3, Tuple4, tuple3, tuple4)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)

-- Basics ----------------------------------------------------------------------

type Lazy a = Unit -> a

-- | A type that can be used to generate values on demand.
data Supply a = Node (Lazy a) (Lazy (Supply a)) (Lazy (Supply a))
derive instance functorSupply :: Functor Supply

-- instance functorSupply :: Functor Supply where
--   map f s = modifySupply s (f <<< supplyValue)

-- | Get the value of a supply.  This function, together with
-- 'modifySupply' forms a comonad on 'Supply'.
supplyValue    :: forall a. Supply a -> a
supplyValue (Node a _ _) = a unit

-- | Generate an infinite list of supplies.
-- split          :: forall a. Supply a -> Array (Supply a)
-- split (Node _ s1 s2)  = A.cons s1.val (split s2.val)

-- | Split a supply into two different supplies.
-- The resulting supplies are different from the input supply.
split2 :: forall a. Supply a -> Tuple (Supply a) (Supply a)
split2 (Node _ s1 s2) = Tuple (s1 unit) (s2 unit)

-- | Split a supply into three different supplies.
split3 :: forall a. Supply a -> Tuple3 (Supply a) (Supply a) (Supply a)
split3 (Node _ s1 s2') =
  let Node _ s2 s3 = s2' unit
  in tuple3 (s1 unit) (s2 unit) (s3 unit)

-- | Split a supply into four different supplies.
split4 :: forall a. Supply a -> Tuple4 (Supply a) (Supply a) (Supply a) (Supply a)
split4 (Node _ s1 s2') =
  let Node _ s2 s3' = s2' unit
      Node _ s3 s4 = s3' unit
  in tuple4 (s1 unit) (s2 unit) (s3 unit) (s4 unit)

-- | Create a new supply
newSupply :: forall a. a -> (a -> a) -> Effect (Supply a)
newSupply start next = do
  r <- Ref.new start
  pure $ gen r
  where
  gen r =
      let v = \_ -> unsafePerformEffect do
            a <- Ref.read r
            Ref.write (next a) r
            pure a
          ls = \_ -> gen r
          rs = \_ -> gen r
          in (Node v ls rs)

-- | Create a new supply and use it without effects
withSupply :: forall r. (Supply Int -> r) -> r
withSupply f = f (unsafePerformEffect (newSupply 0 (_+1)))

-- | Generate a new supply by systematically applying a function
-- to an existing supply.  This function, together with 'supplyValue'
-- form a comonad on 'Supply'.
modifySupply :: forall a b. Supply a -> (Supply a -> b) -> Supply b
modifySupply s f = Node (\_ -> f s) (\_ -> modifySupply (l unit) f) (\_ -> modifySupply (r unit) f)
  where Node _ l r = s
