module FciLisp.Core.Evaluator.Expressions where

import Prelude

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
  VClosure ident body env -> do
    env <- get
    newEnv <- insert ident <$> eval arg <@> env
    case runEvaluator newEnv $ eval body of
      Right value -> pure value
      Left (RuntimeError etype msg) -> fail etype msg
  _ -> fail InvalidApplicationError ""
eval _ = fail SyntaxError $ ""

-- type Ident
--   = String
-- 
-- data Expr
--   = Nil
--   | T
--   | Natural Int
--   | Symbol Ident
--   | Closure Env Ident Ast
--   | GenericClosure Env Ident Ident Ast
--   | Pair Expr Expr
-- 
-- isAtom :: Expr -> Boolean
-- isAtom (Pair _ _) = false
-- isAtom _ = true
-- 
-- fromBool :: Boolean -> Expr
-- fromBool true = T
-- fromBool false = Nil
-- 
-- head :: Expr -> Maybe Expr
-- head (Pair h _) = Just h
-- head _ = Nothing
-- 
-- tail :: Expr -> Maybe Expr
-- tail (Pair _ t) = Just t
-- tail _ = Nothing
-- 
-- instance showExpr :: Show Expr where
--   show Nil = "nil"
--   show T = "t"
--   show (Natural n) = show n
--   show (Symbol x) = x
--   show (Closure _ arg body) = "#<closure (" <> arg <> ") " <> show body <> ")"
--   show (GenericClosure _ f arg body) = "#<genericClosure (" <> f <> " " <> arg <> ") " <> show body <> ")"
--   show (Pair x y) = "(" <> show x <> " . " <> show y <> ")"
-- 
-- instance eqExpr :: Eq Expr where
--   eq Nil Nil = true
--   eq T T = true
--   eq (Symbol x) (Symbol y) = eq x y
--   eq _ _ = false
-- 
-- type Env
--   = Map Ident Expr
-- 
-- initEnv :: Env
-- initEnv = empty
-- 
-- eval :: Ast -> Evaluator Env Expr
-- -- Premitive values
-- eval Empty = pure Nil
-- 
-- eval (Atom ident) | Just n <- fromString ident = pure $ Natural n
-- 
-- eval (Atom ident) =
--   gets (lookup ident) >>= maybe (fail UnboundedVariableError $ "variable '" <> ident <> "'") pure
-- 
-- eval (List (Atom "cons") [ expr1, expr2 ]) =
--   Pair <$> eval expr1 <*> eval expr2
-- 
-- -- Predicates
-- eval (List (Atom "atom") [ expr ]) =
--   fromBool <<< isAtom <$> eval expr
-- 
-- eval (List (Atom "eq") [ expr1, expr2 ]) =
--   fromBool <$> (eq <$> eval expr1 <*> eval expr2)
-- 
-- -- Operations
-- eval (List (Atom "car") [ expr ]) =
--   head <$> eval expr >>= maybe (fail InvalidArgumentsError "in 'car'") pure
-- 
-- eval (List (Atom "cdr") [ expr ]) =
--   tail <$> eval expr >>= maybe (fail InvalidArgumentsError "in 'cdr'") pure
-- 
-- eval (List (Atom "+") exprs) = case exprs # traverse (\expr ->
--     
--   ) of
-- 
-- -- Special forms
-- eval (List (Atom "fun") [ Atom ident, body ]) = Closure <$> get <@> ident <@> body
-- 
-- eval (List (Atom "fix") [ Atom f, Atom ident, body ]) = GenericClosure <$> get <@> f <@> ident <@> body
-- 
-- -- Application
-- eval (List (Atom ident) [ expr ]) = do
--   value <- gets $ lookup ident
--   case value of
--     Just (Closure env ident body) -> do
--       value <- eval expr
--       env <- (insert ident value) <$> get
--       case runEvaluator env $ eval body of
--         Left (RuntimeError etype msg) -> fail etype msg
--         Right value -> pure value
--     Just generic@(GenericClosure env f ident body) -> do
--       value <- eval expr
--       env <- (insert f generic) <$> ((insert ident value) <$> get)
--       case runEvaluator env $ eval body of
--         Left (RuntimeError etype msg) -> fail etype msg
--         Right value -> pure value
--     _ -> fail InvalidApplicationError ""
-- 
-- eval _ = pure Nil


 -- data Value
 --   = Nil
 --   | T
 --   | Symbol String
 --  
 -- | Closure Environment (Array String) Expression
 --   | Closure Environment String Expression
 --   | Pair Value Value
 -- 
 -- instance showValue :: Show Value where
 --   show Nil = "Nil"
 --   show T = "T"
 --   show (Symbol x) = "(Symbol " <> show x <> ")"
 --  
 -- show (Closure _ names body) = "(Closure " <> show names <> " " <> show body <> ")"
 --   show (Closure _ name body) = "(Closure (" <> name <> ") " <> show body <> ")"
 --   show (Pair x y) = "(Pair " <> show x <> " " <> show y <> ")"
 -- 
 -- instance eqValue :: Eq Value where
 --   eq Nil Nil = true
 --   eq T T = true
 --   eq (Symbol x) (Symbol y) = eq x y
 --   eq _ _ = false
 -- 
 -- type Environment
 --   = Map String Value
 -- 
 -- initialEnvironment :: Environment
 -- initialEnvironment = empty
 -- 
 -- evaluator (List [ Atom "fun", (List names), body ]) = case names
 --     # traverse
 --         ( \name -> case name of
 --             Atom ident -> Just ident
 --             _ -> Nothing
 --         ) of
 --   Nothing -> fail SyntaxError "in 'fun'"
 -- Just idents -> Closure <$> get <@> idents <@> body
 -- evaluator (List [ Atom "fun", Atom name, body ]) = Closure <$> get <@> name <@> body
 -- evaluator (List [ Atom "atom", expr ]) = do --   value <- evaluator expr --   case value of --     Pair _ _ -> pure Nil --     _ -> pure T
 -- evaluator (List [ Atom "eq", expr1, expr2 ]) = do --   value1 <- evaluator expr1 --   value2 <- evaluator expr2 --   case (Tuple value1 value2) of --     Tuple Nil Nil -> pure T --     Tuple T T -> pure T --     Tuple (Symbol x) (Symbol y) -> pure $ if eq x y then T else Nil --     _ -> pure Nil
 -- evaluator (List [ Atom "cons", expr1, expr2 ]) = do --   value1 <- evaluator expr1 --   value2 <- evaluator expr2 --   pure $ Pair value1 value2
 -- evaluator (List [ Atom "car", expr ]) = do --   value <- evaluator expr --   case value of --     Pair head _ -> pure head --     _ -> fail InvalidArgumentsError "in 'car'"
 -- evaluator (List [ Atom "cdr", expr ]) = do --   value <- evaluator expr --   case value of --     Pair _ tail -> pure tail --     _ -> fail InvalidArgumentsError "in 'cdr'"
 -- evaluator (List [ Atom name, expr ]) = cons 
 -- evaluator (List exprs) = case uncons exprs of
 --  Nothing -> pure Nil
 --  Just { head: head, tail: tail } -> case head of
 --    Atom ident -> do
 --      value <- gets $ lookup ident
 --      case value of
 --        Just (Closure env names body) -> if length names /= length tail then
 --          fail InvalidNumberOfArgumentsError ""
 --        else do
 --          env <- get
 --          newEnv <- foldM (\env (Tuple k v) -> insert k <$> evaluator v <@> env) env $ zip names tail
 --          case runEvaluator env $ evaluator body of
 --            Left (RuntimeError etype msg) -> fail etype msg
 --            Right value -> pure value
 --        _ -> fail SyntaxError ""
 --    List exprs -> case uncons exprs of
 --      Nothing -> fail InvalidApplicationError ""
 --      Just { head: Atom "fun", tail: tail } -> do
 --        value <- evaluator head
 --        case value of
 --          Closure env names body -> do
 --            newEnv <- foldM (\env (Tuple k v) -> insert k <$> evaluator v <@> env) env $ zip names tail
 --            case runEvaluator env $ evaluator body of
 --              Left (RuntimeError etype msg) -> fail etype msg
 --              Right value -> pure value
 --          _ -> fail InvalidApplicationError ""
 --      _ -> fail InvalidApplicationError ""
 --    _ -> fail InvalidApplicationError ""
 -- evaluator _ = fail SyntaxError ""