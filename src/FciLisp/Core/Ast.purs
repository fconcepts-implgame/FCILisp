module FciLisp.Core.Ast where

import Prelude
import Data.String.Common (joinWith)

data Expression
  = Atom String
  | List (Array Expression)

instance showExpression :: Show Expression where
  show (Atom x) = x
  show (List xs) = "(" <> (xs # map show # joinWith " ") <> ")"

instance eqExpression :: Eq Expression where
  eq (Atom x) (Atom y) = eq x y
  eq (List xs) (List ys) = eq xs ys
  eq _ _ = false