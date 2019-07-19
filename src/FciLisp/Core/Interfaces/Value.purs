module FciLisp.Core.Interfaces.Value
  ( Value(..)
  , eqAsValue
  , isAtom
  , fromBool
  , toBool
  , toNat
  , partialHead
  , partialTail
  , partialLift1
  , partialLift2
  , partialLift2Partial
  , partialLift1Nat
  , partialLift2Nat
  , partialLift2NatPartial
  , partialLift2NatBool
  , partialLiftN
  , partialLiftNNat
  , Env
  , initEnv
  ) where

import Prelude
import Data.Foldable (class Foldable, foldl)
import Data.Natural (Natural)
import Data.Map (Map, empty)
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, traverse)
import FciLisp.Core.Interfaces.Ast (Lisp)

-- First Class Objects
data Value
  = VNil
  | VT
  | VNat Natural
  | VSymbol String
  | VClosure String Lisp Env
  | VRecClosure String String Lisp Env
  | VPair Value Value

-- eq as Lisp's value
-- so this is different from eq of Eq Value
eqAsValue :: Value -> Value -> Boolean
eqAsValue VNil VNil = true

eqAsValue VT VT = true

eqAsValue (VNat x) (VNat y) = eq x y

eqAsValue (VSymbol x) (VSymbol y) = eq x y

eqAsValue _ _ = false

isAtom :: Value -> Boolean
isAtom (VPair _ _) = false

isAtom _ = true

fromBool :: Boolean -> Value
fromBool true = VT

fromBool false = VNil

toBool :: Value -> Boolean
toBool VNil = false

toBool _ = true

toNat :: Value -> Maybe Natural
toNat (VNat n) = Just n

toNat _ = Nothing

partialHead :: Value -> Maybe Value
partialHead (VPair h _) = Just h

partialHead _ = Nothing

partialTail :: Value -> Maybe Value
partialTail (VPair _ t) = Just t

partialTail _ = Nothing

type Env
  = Map String Value

initEnv :: Env
initEnv = empty

instance showValue :: Show Value where
  show VNil = "nil"
  show VT = "t"
  show (VNat n) = show n
  show (VSymbol ident) = ident
  show (VClosure _ _ _) = "<closure>"
  show (VRecClosure _ _ _ _) = "<recursive closure>"
  show (VPair x y) = "(" <> show x <> " . " <> show y <> ")"

instance eqValue :: Eq Value where
  eq VNil VNil = true
  eq VT VT = true
  eq (VNat x) (VNat y) = eq x y
  eq (VSymbol x) (VSymbol y) = eq x y
  eq (VClosure arg1 body1 env1) (VClosure arg2 body2 env2) = eq arg1 arg2 && eq body1 body2 && eq env1 env2
  eq (VRecClosure fName1 arg1 body1 env1) (VRecClosure fName2 arg2 body2 env2) = eq fName1 fName2 && eq arg1 arg2 && eq body1 body2 && eq env1 env2
  eq (VPair x1 y1) (VPair x2 y2) = eq x1 x2 && eq y1 y2
  eq _ _ = false

partialLift1 :: forall a b. (Value -> Maybe a) -> (b -> Value) -> (a -> b) -> Value -> Maybe Value
partialLift1 to from f x = f <$> to x <#> from

partialLift2 :: forall a b c. (Value -> Maybe a) -> (Value -> Maybe b) -> (c -> Value) -> (a -> b -> c) -> Value -> Value -> Maybe Value
partialLift2 to1 to2 from f x y = f <$> to1 x <*> to2 y <#> from

partialLift2Partial :: forall a b c. (Value -> Maybe a) -> (Value -> Maybe b) -> (c -> Value) -> (a -> b -> Maybe c) -> Value -> Value -> Maybe Value
partialLift2Partial to1 to2 from f x y = f <$> to1 x <*> to2 y >>= (map from)

partialLift1Nat :: (Natural -> Natural) -> Value -> Maybe Value
partialLift1Nat = partialLift1 toNat VNat

partialLift2Nat :: (Natural -> Natural -> Natural) -> Value -> Value -> Maybe Value
partialLift2Nat = partialLift2 toNat toNat VNat

partialLift2NatPartial :: (Natural -> Natural -> Maybe Natural) -> Value -> Value -> Maybe Value
partialLift2NatPartial = partialLift2Partial toNat toNat VNat

partialLift2NatBool :: (Natural -> Natural -> Boolean) -> Value -> Value -> Maybe Value
partialLift2NatBool = partialLift2 toNat toNat fromBool

partialLiftN :: forall a f. Foldable f => Traversable f => Semiring a => (Value -> Maybe a) -> (a -> Value) -> (a -> a -> a) -> f Value -> Maybe Value
partialLiftN to from f xs = xs # traverse to <#> foldl f zero <#> from

partialLiftNNat :: forall f. Foldable f => Traversable f => (Natural -> Natural -> Natural) -> f Value -> Maybe Value
partialLiftNNat = partialLiftN toNat VNat
