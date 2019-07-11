module FciLisp.Core.Evaluator.Expressions where

import Prelude
import Data.Array (uncons, mapMaybe, length, zip, foldM)
import Data.Either (Either(..), either)
import Data.Map (Map(..), lookup, insert, empty)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import FciLisp.Core.Ast (Expression(..))
import FciLisp.Core.Evaluator.Class (ErrorType(..), Evaluator(..), runEvaluator, fail, get, gets, modify_, RuntimeError(..))

-- import FciLisp.Core.Evaluator.Data (Value(..), Atom(..), Pair(..))
data Value
  = Nil
  | T
  | Symbol String
  | Closure Environment (Array String) Expression
  | Pair Value Value

instance showValue :: Show Value where
  show Nil = "Nil"
  show T = "T"
  show (Symbol x) = "(Symbol " <> show x <> ")"
  show (Closure _ names body) = "(Closure " <> show names <> " " <> show body <> ")"
  show (Pair x y) = "(Pair " <> show x <> " " <> show y <> ")"

instance eqValue :: Eq Value where
  eq Nil Nil = true
  eq T T = true
  eq (Symbol x) (Symbol y) = eq x y
  eq _ _ = false

type Environment
  = Map String Value

initialEnvironment :: Environment
initialEnvironment = empty

evaluator :: Expression -> Evaluator Environment Value
evaluator (Atom ident) = do
  value <- gets $ lookup ident
  case value of
    Nothing -> fail UnboundedVariableError $ "variable '" <> show ident <> "'"
    Just v -> pure v

evaluator (List []) = pure $ Nil

evaluator (List [ Atom "def", Atom ident, expr ]) = do
  definedValue <- gets $ lookup ident
  case definedValue of
    Just _ -> fail RedefinedError "in 'def'"
    Nothing -> do
      value <- evaluator expr
      modify_ $ insert ident value
  pure $ Symbol ident

evaluator (List [ Atom "fun", (List names), body ]) = case names
    # traverse
        ( \name -> case name of
            Atom ident -> Just ident
            _ -> Nothing
        ) of
  Nothing -> fail SyntaxError "in 'fun'"
  Just idents -> Closure <$> get <@> idents <@> body

evaluator (List [ Atom "atom", expr ]) = do
  value <- evaluator expr
  case value of
    Pair _ _ -> pure Nil
    _ -> pure T

evaluator (List [ Atom "eq", expr1, expr2 ]) = do
  value1 <- evaluator expr1
  value2 <- evaluator expr2
  case (Tuple value1 value2) of
    Tuple Nil Nil -> pure T
    Tuple T T -> pure T
    Tuple (Symbol x) (Symbol y) -> pure $ if eq x y then T else Nil
    _ -> pure Nil

evaluator (List [ Atom "cons", expr1, expr2 ]) = do
  value1 <- evaluator expr1
  value2 <- evaluator expr2
  pure $ Pair value1 value2

evaluator (List [ Atom "car", expr ]) = do
  value <- evaluator expr
  case value of
    Pair head _ -> pure head
    _ -> fail InvalidArgumentsError "in 'car'"

evaluator (List [ Atom "cdr", expr ]) = do
  value <- evaluator expr
  case value of
    Pair _ tail -> pure tail
    _ -> fail InvalidArgumentsError "in 'cdr'"

evaluator (List exprs) = case uncons exprs of
  Nothing -> pure Nil
  Just { head: head, tail: tail } -> case head of
    Atom ident -> do
      value <- gets $ lookup ident
      case value of
        Just (Closure env names body) -> if length names /= length tail then
          fail InvalidNumberOfArgumentsError ""
        else do
          env <- get
          newEnv <- foldM (\env (Tuple k v) -> insert k <$> evaluator v <@> env) env $ zip names tail
          case runEvaluator env $ evaluator body of
            Left (RuntimeError etype msg) -> fail etype msg
            Right value -> pure value
        _ -> fail SyntaxError ""
    List exprs -> case uncons exprs of
      Nothing -> fail InvalidApplicationError ""
      Just { head: Atom "fun", tail: tail } -> do
        value <- evaluator head
        case value of
          Closure env names body -> do
            newEnv <- foldM (\env (Tuple k v) -> insert k <$> evaluator v <@> env) env $ zip names tail
            case runEvaluator env $ evaluator body of
              Left (RuntimeError etype msg) -> fail etype msg
              Right value -> pure value
          _ -> fail InvalidApplicationError ""
      _ -> fail InvalidApplicationError ""
    _ -> fail InvalidApplicationError ""

evaluator _ = fail SyntaxError ""