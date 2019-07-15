module Data.Natural
  ( Natural()
  , fromInt
  , partialSub
  , (-.)
  , partialDiv
  , (/.)
  , partialMod
  ) where

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
  pred n = if n == zero then Nothing else Just $ n -. one

instance semiringNatural :: Semiring Natural where
  one = Natural 1
  mul (Natural m) (Natural n) = Natural $ m * n
  zero = Natural 0
  add (Natural m) (Natural n) = Natural $ m + n

derive newtype instance eqNatural :: Eq Natural

derive newtype instance ordNatural :: Ord Natural

derive newtype instance showNatural :: Show Natural

partialSub :: Natural -> Natural -> Natural
partialSub (Natural m) (Natural n) = Natural $ m - n

infixl 6 partialSub as -.

partialDiv :: Natural -> Natural -> Maybe Natural
partialDiv (Natural m) (Natural n) = if n /= 0 then Just $ Natural $ m / n else Nothing

infixl 7 partialDiv as /.

partialMod :: Natural -> Natural -> Maybe Natural
partialMod (Natural m) (Natural n) = if n /= 0 then Just $ Natural $ m `mod` n else Nothing
