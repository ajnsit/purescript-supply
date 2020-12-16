module Supply where

import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Data.CommutativeRing ((+))
import Data.Function (($))
import Data.Functor (class Functor)
import Data.Lazy (Lazy)
import Data.Lazy as Lazy
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (Tuple3, Tuple4, tuple3, tuple4)
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)

-- Basics ----------------------------------------------------------------------

-- | A type that can be used to generate values on demand.
data Supply a = Node (Lazy a) (Lazy (Supply a)) (Lazy (Supply a))
derive instance functorSupply :: Functor Supply

-- | Get the value of a supply.  This function, together with
-- 'modifySupply' forms a comonad on 'Supply'.
supplyValue    :: forall a. Supply a -> a
supplyValue (Node a _ _) = Lazy.force a

-- | Generate an infinite list of supplies.
-- split          :: forall a. Supply a -> Array (Supply a)
-- split (Node _ s1 s2)  = A.cons s1.val (split s2.val)

-- | Split a supply into two different supplies.
-- The resulting supplies are different from the input supply.
split2 :: forall a. Supply a -> Tuple (Supply a) (Supply a)
split2 (Node _ s1 s2) = Tuple (Lazy.force s1) (Lazy.force s2)

-- | Split a supply into three different supplies.
split3 :: forall a. Supply a -> Tuple3 (Supply a) (Supply a) (Supply a)
split3 (Node _ s1 s2') =
  let Node _ s2 s3 = Lazy.force s2'
  in tuple3 (Lazy.force s1) (Lazy.force s2) (Lazy.force s3)

-- | Split a supply into four different supplies.
split4 :: forall a. Supply a -> Tuple4 (Supply a) (Supply a) (Supply a) (Supply a)
split4 (Node _ s1 s2') =
  let Node _ s2 s3' = Lazy.force s2'
      Node _ s3 s4 = Lazy.force s3'
  in tuple4 (Lazy.force s1) (Lazy.force s2) (Lazy.force s3) (Lazy.force s4)

unsafeInterleaveEffect :: forall a. Effect a -> Effect (Lazy a)
unsafeInterleaveEffect eff = pure $ Lazy.defer \_ -> unsafePerformEffect eff

-- | Create a new supply
newSupply :: forall a. a -> (a -> a) -> Effect (Supply a)
newSupply start next = do
  r <- Ref.new start
  gen r
  where
  gen r = do
    v <- unsafeInterleaveEffect do
      a <- Ref.read r
      Ref.write (next a) r
      pure a
    ls <- unsafeInterleaveEffect $ gen r
    rs <- unsafeInterleaveEffect $ gen r
    pure $ Node v ls rs

-- | Create a new supply and use it without effects
withSupply :: forall r. (Supply Int -> r) -> r
withSupply f = f (unsafePerformEffect (newSupply 0 (_+1)))

-- | Generate a new supply by systematically applying a function
-- to an existing supply.  This function, together with 'supplyValue'
-- form a comonad on 'Supply'.
modifySupply :: forall a b. Supply a -> (Supply a -> b) -> Supply b
modifySupply s f = Node (Lazy.defer \_ -> f s) (Lazy.defer \_ -> modifySupply (Lazy.force l) f) (Lazy.defer \_ -> modifySupply (Lazy.force r) f)
  where Node _ l r = s
