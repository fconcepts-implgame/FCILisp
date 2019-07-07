module Core.Ast where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.List (List(..), findMap, (:))
import Data.Traversable (traverse)

data Expression
    = Atom String
    | List (List Expression)

instance showExpression :: Show Expression where
    show (Atom x) = "(Atom " <> show x <> ")"
    show (List xs) = "(List " <> show xs <> ")"

instance eqExpression :: Eq Expression where
    eq (Atom x) (Atom y) = eq x y
    eq (List xs) (List ys) = eq xs ys
    eq _ _ = false

t :: Expression
t = Atom "t"

nil :: Expression
nil = List Nil

data Error = Error ErrorType String

instance showError :: Show Error where
    show (Error etype msg) = "(Error " <> show etype <> ": " <> msg <> ")"

derive instance eqError :: Eq Error

data ErrorType
    = InvalidArgumentsError
    | SyntaxError
    | ConditionCoverageError

instance showErrorType :: Show ErrorType where
    show InvalidArgumentsError = "InvalidArgumentsError"
    show SyntaxError = "SyntaxError"
    show ConditionCoverageError = "ConditionCoverageError"

derive instance eqErrorType :: Eq ErrorType

eval :: Expression -> Either Error Expression
-- Literals
eval (List Nil) = Right nil
eval (Atom "t") = Right t
-- Constructors
eval (List (Cons (Atom "cons") (Cons expr1 (Cons expr2 Nil)))) = do
    fixed1 <- eval expr1
    fixed2 <- eval expr2
    Right $ List $ (Atom "cons"):fixed1:fixed2:Nil
-- Quote
eval (List (Cons (Atom "quote") (Cons expr Nil))) = Right expr
-- Predicates
eval (List (Cons (Atom "atom") (Cons expr Nil))) = do
  fixed <- eval expr
  case fixed of
    Atom _ -> Right t
    List Nil -> Right t
    _ -> Right nil
eval (List (Cons (Atom "eq") (Cons expr1 (Cons expr2 Nil)))) = do
  fixed1 <- eval expr1
  fixed2 <- eval expr2
  case { fixed1, fixed2 } of
    { fixed1: Atom str1, fixed2: Atom str2 } -> Right $ if str1 == str2 then t else nil
    { fixed1: List Nil, fixed2: List Nil } -> Right t
    _ -> Right nil
-- Operators
eval (List (Cons (Atom "car") (Cons x Nil))) = do
  fixedX <- eval x
  case fixedX of
    List (Cons (Atom "cons") (Cons head (Cons _ Nil))) -> Right head
    List Nil -> Left $ Error InvalidArgumentsError "in 'car'"
    _ -> Left $ Error InvalidArgumentsError "in 'car'"
eval (List (Cons (Atom "cdr") (Cons x Nil))) = do
  fixedX <- eval x
  case fixedX of
    List (Cons (Atom "cons") (Cons _ (Cons tail Nil))) -> Right tail
    List Nil -> Left $ Error InvalidArgumentsError "in 'cdr'"
    _ -> Left $ Error InvalidArgumentsError "in 'cdr'"
eval (List (Cons (Atom "cond") pairExprs)) = case pairExprs # traverse
  (\pairExpr -> case pairExpr of
    List (Cons cond (Cons expr Nil)) -> Just { cond, expr }
    _ -> Nothing
  ) of
    Nothing -> Left $ Error InvalidArgumentsError "in 'cond'"
    Just pairs -> case pairs # findMap
      (\{ cond, expr } -> case eval cond of
        left@(Left _) -> Just left
        Right (List Nil) -> Nothing
        Right _ -> Just $ Right expr
      ) of
        Nothing -> Left $ Error ConditionCoverageError "in 'cond'"
        Just left@(Left _) -> left
        Just (Right expr) -> eval expr

eval _ = Left $ Error SyntaxError ""


