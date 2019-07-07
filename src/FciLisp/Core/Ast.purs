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

data Environment
  = Environment (List { name :: String, value :: Expression })

eval :: Expression -> Environment -> Either Error { expr :: Expression, env :: Environment }
-- Literals
eval (List Nil) env = Right { expr: nil, env: env }
eval (Atom "t") env = Right { expr: t, env: env }
-- Constructors
eval (List (Cons (Atom "cons") (Cons expr1 (Cons expr2 Nil)))) env = do
    fixed1 <- eval expr1 env
    fixed2 <- eval expr2 env
    Right $ { expr: List $ (Atom "cons"):(fixed1.expr):(fixed2.expr):Nil, env: env }
-- Quote
eval (List (Cons (Atom "quote") (Cons expr Nil))) env = Right { expr, env }
-- Predicates
eval (List (Cons (Atom "atom") (Cons expr Nil))) env = do
  fixed <- eval expr env
  case fixed.expr of
    Atom _ -> Right { expr: t, env: env }
    List Nil -> Right { expr: t, env: env }
    _ -> Right { expr: nil, env: env }
eval (List (Cons (Atom "eq") (Cons expr1 (Cons expr2 Nil)))) env = do
  fixed1 <- eval expr1 env
  fixed2 <- eval expr2 env
  case { fixed1: fixed1.expr, fixed2: fixed2.expr } of
    { fixed1: Atom str1, fixed2: Atom str2 } -> Right { expr: if str1 == str2 then t else nil, env: env }
    { fixed1: List Nil, fixed2: List Nil } -> Right { expr: t, env: env }
    _ -> Right { expr : nil, env: env }
-- Operators
eval (List (Cons (Atom "car") (Cons x Nil))) env = do
  fixedX <- eval x env
  case fixedX.expr of
    List (Cons (Atom "cons") (Cons head (Cons _ Nil))) -> Right { expr: head, env: env }
    List Nil -> Left $ Error InvalidArgumentsError "in 'car'"
    _ -> Left $ Error InvalidArgumentsError "in 'car'"
eval (List (Cons (Atom "cdr") (Cons x Nil))) env = do
  fixedX <- eval x env
  case fixedX.expr of
    List (Cons (Atom "cons") (Cons _ (Cons tail Nil))) -> Right { expr: tail, env: env }
    List Nil -> Left $ Error InvalidArgumentsError "in 'cdr'"
    _ -> Left $ Error InvalidArgumentsError "in 'cdr'"
eval (List (Cons (Atom "cond") pairExprs)) env = case pairExprs # traverse
  (\pairExpr -> case pairExpr of
    List (Cons cond (Cons expr Nil)) -> Just { cond, expr }
    _ -> Nothing
  ) of
    Nothing -> Left $ Error InvalidArgumentsError "in 'cond'"
    Just pairs -> case pairs # findMap
      (\{ cond, expr } -> case eval cond env of
        left@(Left _) -> Just left
        Right { expr: (List Nil), env: _ } -> Nothing
        Right { expr: _, env: env } -> Just $ Right { expr, env }
      ) of
        Nothing -> Left $ Error ConditionCoverageError "in 'cond'"
        Just left@(Left _) -> left
        Just (Right { expr, env }) -> eval expr env

eval _ env = Left $ Error SyntaxError ""


