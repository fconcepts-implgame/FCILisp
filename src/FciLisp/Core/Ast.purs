module FciLisp.Core.Ast (Lisp(..)) where

import Prelude hiding (Ordering(..))
import Data.Array as A
import Data.List (List, (:))
import Data.Natural (Natural)
import Data.String.Common (joinWith)

data Lisp
  = LNil
  | LT
  | LNat Natural
  | LSymbol String
  | LList Lisp (List Lisp)

instance showLisp :: Show Lisp where
  show LNil = "Nil"
  show LT = "T"
  show (LNat n) = "(Nat " <> show n <> ")"
  show (LSymbol ident) = "(Symbol " <> ident <> ")"
  show (LList x xs) = "(List " <> (x : xs # map show # A.fromFoldable # joinWith " ") <> ")"

instance eqLisp :: Eq Lisp where
  eq LNil LNil = true
  eq LT LT = true
  eq (LNat x) (LNat y) = eq x y
  eq (LSymbol x) (LSymbol y) = eq x y
  eq (LList x xs) (LList y ys) = eq x y && eq xs ys
  eq _ _ = false
