module FciLisp.Core.Ast where

import Prelude hiding (Ordering(..))
import Data.Array as A
import Data.List (List)
import Data.Natural (Natural)
import Data.String.Common (joinWith)

data Lisp
  = LNil
  | LT
  | LNat Natural
  | LSymbol Ident
  | LError String Lisp
  | LList Lisp (List Lisp)

type Ident
  = String

instance showLisp :: Show Lisp where
  show LNil = "nil"
  show LT = "t"
  show (LNat n) = show n
  show (LSymbol ident) = show ident
  show (LError msg code) = "<error: " <> msg <> ">"
  show (LList x xs) = "(" <> show x <> " " <> (xs # map show # A.fromFoldable # joinWith " ") <> ")"

instance eqLisp :: Eq Lisp where
  eq LNil LNil = true
  eq LT LT = true
  eq (LNat x) (LNat y) = eq x y
  eq (LSymbol x) (LSymbol y) = eq x y
  eq (LError msg1 code1) (LError msg2 code2) = eq msg1 msg2 && eq code1 code2
  eq (LList x xs) (LList y ys) = eq x y && eq xs ys
  eq _ _ = false
