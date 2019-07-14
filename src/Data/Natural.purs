module Data.Natural where

import Prelude
import Data.Enum (class Enum)
import Data.Maybe (Maybe(..))

newtype Natural
  = Natural Int

fromInt :: Int -> Natural
fromInt n
  | n >= 0 = Natural n

fromInt _ = Natural 0

instance enumNatural :: Enum Natural where
  succ n = Just $ n + one
  pred n = if n == zero then Nothing else Just $ minus n one

instance semiringNatural :: Semiring Natural where
  one = Natural 1
  mul (Natural m) (Natural n) = Natural $ m * n
  zero = Natural 0
  add (Natural m) (Natural n) = Natural $ m + n

derive newtype instance eqNatural :: Eq Natural

derive newtype instance ordNatural :: Ord Natural

derive newtype instance showNatural :: Show Natural

minus :: Natural -> Natural -> Natural
minus (Natural m) (Natural n) = fromInt $ m - n
