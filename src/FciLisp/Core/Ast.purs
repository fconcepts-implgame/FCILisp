module Core.Ast where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.List (List(..), findMap, (:))
import Data.Tuple (Tuple(..))
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

data Error
  = Error ErrorType String

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

data Environment
  = Environment (List { name :: String, value :: Expression })

eval :: Expression -> Environment -> Either Error (Tuple Expression Environment)
-- Literals
eval (List Nil) env = Right $ Tuple nil env

eval (Atom "t") env = Right $ Tuple t env

-- Constructors
eval (List (Cons (Atom "cons") (Cons expr1 (Cons expr2 Nil)))) env = do
  (Tuple fixed1 _) <- eval expr1 env
  (Tuple fixed2 _) <- eval expr2 env
  Right $ Tuple (List $ (Atom "cons") : fixed1 : fixed2 : Nil) env

-- Quote
eval (List (Cons (Atom "quote") (Cons expr Nil))) env = Right $ Tuple expr env

-- Predicates
eval (List (Cons (Atom "atom") (Cons expr Nil))) env = do
  (Tuple fixed _) <- eval expr env
  case fixed of
    Atom _ -> Right $ Tuple t env
    List Nil -> Right $ Tuple t env
    _ -> Right $ Tuple nil env

eval (List (Cons (Atom "eq") (Cons expr1 (Cons expr2 Nil)))) env = do
  (Tuple fixed1 _) <- eval expr1 env
  (Tuple fixed2 _) <- eval expr2 env
  case (Tuple fixed1 fixed2) of
    Tuple (Atom str1) (Atom str2) -> Right $ Tuple (if str1 == str2 then t else nil) env
    Tuple (List Nil) (List Nil) -> Right $ Tuple t env
    _ -> Right $ Tuple nil env

-- Operators
eval (List (Cons (Atom "car") (Cons x Nil))) env = do
  (Tuple fixedX _) <- eval x env
  case fixedX of
    List (Cons (Atom "cons") (Cons head (Cons _ Nil))) -> Right $ Tuple head env
    List Nil -> Left $ Error InvalidArgumentsError "in 'car'"
    _ -> Left $ Error InvalidArgumentsError "in 'car'"

eval (List (Cons (Atom "cdr") (Cons x Nil))) env = do
  (Tuple fixedX _) <- eval x env
  case fixedX of
    List (Cons (Atom "cons") (Cons _ (Cons tail Nil))) -> Right $ Tuple tail env
    List Nil -> Left $ Error InvalidArgumentsError "in 'cdr'"
    _ -> Left $ Error InvalidArgumentsError "in 'cdr'"

eval (List (Cons (Atom "cond") pairExprs)) env = case pairExprs
    # traverse
        ( \pairExpr -> case pairExpr of
            List (Cons cond (Cons expr Nil)) -> Just { cond, expr }
            _ -> Nothing
        ) of
  Nothing -> Left $ Error InvalidArgumentsError "in 'cond'"
  Just pairs -> case pairs
      # findMap
          ( \{ cond, expr } -> case eval cond env of
              left@(Left _) -> Just left
              Right (Tuple (List Nil) _) -> Nothing
              Right (Tuple _ env) -> Just $ Right $ Tuple expr env
          ) of
    Nothing -> Left $ Error ConditionCoverageError "in 'cond'"
    Just left@(Left _) -> left
    Just (Right (Tuple expr env)) -> eval expr env

eval _ env = Left $ Error SyntaxError ""
