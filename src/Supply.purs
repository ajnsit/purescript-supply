module Supply where

import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Comonad (class Comonad)
import Control.Extend (class Extend)
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
-- | `s` is a phantom type parameter that prevents unsafe usage
data Supply s a = Node (Lazy a) (Lazy (Supply s a)) (Lazy (Supply s a))
derive instance functorSupply :: Functor (Supply s)

-- | Get the value of a supply.  This function, together with
-- 'modifySupply' forms a comonad on 'Supply'.
supplyValue    :: forall s a. Supply s a -> a
supplyValue (Node a _ _) = Lazy.force a

-- | Split a supply into two different supplies.
-- The resulting supplies are different from the input supply.
split2 :: forall s a. Supply s a -> Tuple (Supply s a) (Supply s a)
split2 (Node _ s1 s2) = Tuple (Lazy.force s1) (Lazy.force s2)

-- | Split a supply into three different supplies.
split3 :: forall s a. Supply s a -> Tuple3 (Supply s a) (Supply s a) (Supply s a)
split3 (Node _ s1 s2') =
  let Node _ s2 s3 = Lazy.force s2'
  in tuple3 (Lazy.force s1) (Lazy.force s2) (Lazy.force s3)

-- | Split a supply into four different supplies.
split4 :: forall s a. Supply s a -> Tuple4 (Supply s a) (Supply s a) (Supply s a) (Supply s a)
split4 (Node _ s1 s2') =
  let Node _ s2 s3' = Lazy.force s2'
      Node _ s3 s4 = Lazy.force s3'
  in tuple4 (Lazy.force s1) (Lazy.force s2) (Lazy.force s3) (Lazy.force s4)

-- | unsafeInterleaveEffect allows an Effect to be deferred lazily.
--   The Effect is only performed when the result is forced.
-- TODO: Put this in purescript-effect
unsafeInterleaveEffect :: forall a. Effect a -> Effect (Lazy a)
unsafeInterleaveEffect eff = pure $ Lazy.defer \_ -> unsafePerformEffect eff

-- | Create a new supply
newSupply :: forall s a. a -> (a -> a) -> Effect (Supply s a)
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
withSupply :: forall r a. a -> (a -> a) -> (forall s. Supply s a -> r) -> r
withSupply start next action = action $ unsafePerformEffect (newSupply start next)

-- | Generate a new supply by systematically applying a function
-- to an existing supply.  This function, together with 'supplyValue'
-- forms a comonad on 'Supply'.
modifySupply :: forall s a b. Supply s a -> (Supply s a -> b) -> Supply s b
modifySupply s f = Node (Lazy.defer \_ -> f s) (Lazy.defer \_ -> modifySupply (Lazy.force l) f) (Lazy.defer \_ -> modifySupply (Lazy.force r) f)
  where Node _ l r = s

instance extendSupply :: Extend (Supply s) where
  extend f s = modifySupply s f

instance comonadSupply :: Comonad (Supply s) where
  extract = supplyValue

-- Specialised supplies for convenience

-- | Create a new integer supply
newIntSupply :: forall s. Effect (Supply s Int)
newIntSupply = newSupply 0 (_+1)

-- | Create a new integer supply and use it without effects
withIntSupply :: forall r. (forall s. Supply s Int -> r) -> r
withIntSupply = withSupply 0 (_+1)
