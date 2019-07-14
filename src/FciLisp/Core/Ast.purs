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
  show LNil = "nil"
  show LT = "t"
  show (LNat n) = show n
  show (LSymbol ident) = ident
  show (LList x xs) = "(" <> (x : xs # map show # A.fromFoldable # joinWith " ") <> ")"

instance eqLisp :: Eq Lisp where
  eq LNil LNil = true
  eq LT LT = true
  eq (LNat x) (LNat y) = eq x y
  eq (LSymbol x) (LSymbol y) = eq x y
  eq (LList x xs) (LList y ys) = eq x y && eq xs ys
  eq _ _ = false
