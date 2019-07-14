module FciLisp.Core.Evaluator.Expressions where

import Prelude

import Control.Monad.Except (throwError)
import Data.Either (Either(..), either)
import Data.Foldable (sum)
import Data.Int (fromString)
import Data.List (List(..))
import Data.Map (Map(..), lookup, insert, empty)
import Data.Maybe (Maybe(..), maybe)
import Data.Natural (Natural, (-.))
import Data.Newtype (overF)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import FciLisp.Core.Ast (Lisp(..))
import FciLisp.Core.Evaluator.Class (ErrorType(..), Evaluator(..), RuntimeError(..), fail, get, gets, modify_, runEvaluator)

data Value
  = VNil
  | VT
  | VNat Natural
  | VSymbol Ident
  | VClosure Ident Lisp Env
  | VRecClosure Ident Ident Lisp Env
  | VPair Value Value

fromBool :: Boolean -> Value
fromBool true = VT
fromBool false = VNil

type Env = Map Ident Value

initEnv :: Env
initEnv = empty

type Ident = String

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
  eq _ _ = false

eval :: Lisp -> Evaluator Env Value
-- Literals and Constructors
eval LNil = pure VNil
eval LT = pure VT
eval (LNat n) = pure $ VNat n
eval (LError msg code) = fail SyntaxError $ msg <> " in '" <> show code
eval (LSymbol ident) =
  gets (lookup ident) >>= maybe (fail UnboundedVariableError $ "variable '" <> ident) pure
eval (LList (LSymbol "fun") (Cons (LSymbol ident) (Cons body Nil))) = VClosure ident body <$> get
eval (LList (LSymbol "fun") _) = fail InvalidNumberOfArgumentsError "in 'fun"
eval (LList (LSymbol "fix") (Cons (LSymbol funName) (Cons (LSymbol ident) (Cons body Nil)))) = VRecClosure funName ident body <$> get
eval (LList (LSymbol "fix") _) = fail InvalidNumberOfArgumentsError "in 'fix"
eval (LList (LSymbol "cons") (Cons x (Cons y Nil))) = VPair <$> eval x <*> eval y
eval (LList (LSymbol "cons") _) = fail InvalidNumberOfArgumentsError "in 'cons"
-- Predicatres
eval (LList (LSymbol "atom?") (Cons x Nil)) = eval x >>= \v -> case v of
  (VPair _ _) -> pure VNil
  _ -> pure VT
eval (LList (LSymbol "atom?") _) = fail InvalidNumberOfArgumentsError "in 'atom?"
eval (LList (LSymbol "eq?") (Cons x (Cons y Nil))) = eq <$> eval x <*> eval y >>= pure <<< fromBool
eval (LList (LSymbol "eq?") _) = fail InvalidNumberOfArgumentsError "in 'eq?"
-- Operators
eval (LList (LSymbol "head") (Cons x Nil)) = eval x >>= \v -> case v of
  (VPair h _) -> pure h
  _ -> fail InvalidArgumentsError "in 'head"
  
eval (LList (LSymbol "head") _) = fail InvalidNumberOfArgumentsError "in 'head"
eval (LList (LSymbol "tail") (Cons x Nil)) = eval x >>= \v -> case v of
    (VPair _ t) -> pure t
    _ -> fail InvalidArgumentsError "in 'tail"
eval (LList (LSymbol "tail") _) = fail InvalidNumberOfArgumentsError "in 'tail"
eval (LList (LSymbol "+") (Cons x (Cons y Nil))) = Tuple <$> eval x <*> eval y >>= \t -> case t of
  Tuple (VNat m) (VNat n) -> pure $ VNat $ m + n
  _ -> fail InvalidArgumentsError "in '+"
eval (LList (LSymbol "+") _) = fail InvalidNumberOfArgumentsError "in '+"
eval (LList (LSymbol "-") (Cons x (Cons y Nil))) = Tuple <$> eval x <*> eval y >>= \t -> case t of
  Tuple (VNat m) (VNat n) -> pure $ VNat $ m -. n
  _ -> fail InvalidArgumentsError "in '-"
eval (LList (LSymbol "-") _) = fail InvalidNumberOfArgumentsError "in '-"
eval (LList (LSymbol "<") (Cons x (Cons y Nil))) = Tuple <$> eval x <*> eval y >>= \t -> case t of
  Tuple (VNat m) (VNat n) -> pure $ fromBool $ m < n
  _ -> fail InvalidArgumentsError "in '<"
eval (LList (LSymbol "<") _) = fail InvalidNumberOfArgumentsError "in '<"
eval (LList (LSymbol ">") (Cons x (Cons y Nil))) = Tuple <$> eval x <*> eval y >>= \t -> case t of
  Tuple (VNat m) (VNat n) -> pure $ fromBool $ m > n
  _ -> fail InvalidArgumentsError "in '>"
-- Applications
eval (LList f (Cons arg Nil)) = eval f >>= \v -> case v of
  VClosure ident body env ->
    runEvaluator <$> (insert ident <$> eval arg <*> get) <@> eval body >>= either throwError pure
  _ -> fail InvalidApplicationError ""
eval _ = fail SyntaxError $ ""
