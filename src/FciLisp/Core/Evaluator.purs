module FciLisp.Core.Evaluator
  ( eval
  ) where

import Prelude hiding (Ordering(..))
import Data.List (List(..))
import Data.Map (lookup, insert)
import Data.Maybe (Maybe, maybe)
import Data.Natural (Natural, (-.), (/.), partialMod)
import FciLisp.Core.Evaluator.Class (Evaluator, fromEither, get, gets, runEvaluator)
import FciLisp.Core.Interfaces.Ast (Lisp(..))
import FciLisp.Core.Interfaces.RuntimeError (ErrorType(..), fail)
import FciLisp.Core.Interfaces.Value (Value(..), eqAsValue, isAtom, fromBool, toBool, partialHead, partialTail, partialLift2Nat, partialLift2NatPartial, partialLift2NatBool, Env)

eval :: Lisp -> Evaluator Env Value
-- Literals and Constructors
eval LNil = pure VNil

eval LT = pure VT

eval (LNat n) = pure $ VNat n

eval ast@(LSymbol ident) = gets (lookup ident) >>= maybe (fail UnboundedVariableError $ "symbol '" <> ident <> " in '" <> show ast) pure

eval (LList (LSymbol "fun") (Cons (LSymbol ident) (Cons body Nil))) = VClosure ident body <$> get

eval ast@(LList (LSymbol "fun") _) = fail InvalidNumberOfArgumentsError $ "constructor 'fun in '" <> show ast

eval (LList (LSymbol "recur") (Cons (LSymbol funName) (Cons (LSymbol ident) (Cons body Nil)))) = VRecClosure funName ident body <$> get

eval ast@(LList (LSymbol "recur") _) = fail InvalidNumberOfArgumentsError $ "constructor 'recur in '" <> show ast

eval (LList (LSymbol "cons") (Cons x (Cons y Nil))) = VPair <$> eval x <*> eval y

eval ast@(LList (LSymbol "cons") _) = fail InvalidNumberOfArgumentsError $ "constructor 'cons in '" <> show ast

-- Predicatres
eval (LList (LSymbol "atom?") (Cons x Nil)) = isAtom <$> eval x <#> fromBool

eval ast@(LList (LSymbol "atom?") _) = fail InvalidNumberOfArgumentsError $ "predicate 'atom? in '" <> show ast

eval (LList (LSymbol "eq?") (Cons x (Cons y Nil))) = eqAsValue <$> eval x <*> eval y <#> fromBool

eval ast@(LList (LSymbol "eq?") _) = fail InvalidNumberOfArgumentsError $ "predicate 'eq? in '" <> show ast

-- Operators
eval ast@(LList (LSymbol "head") (Cons x Nil)) = partialHead <$> eval x >>= maybe (fail InvalidArgumentsError $ "operator 'head in '" <> show ast) pure

eval ast@(LList (LSymbol "head") _) = fail InvalidNumberOfArgumentsError $ "operator 'head in '" <> show ast

eval ast@(LList (LSymbol "tail") (Cons x Nil)) = partialTail <$> eval x >>= maybe (fail InvalidArgumentsError $ "operator 'tail in '" <> show ast) pure

eval ast@(LList (LSymbol "tail") _) = fail InvalidNumberOfArgumentsError $ "operator 'tail in '" <> show ast

eval ast@(LList (LSymbol "+") (Cons x (Cons y Nil))) = eval2Nat (+) x y >>= maybe (fail InvalidArgumentsError $ "operator '+ in '" <> show ast) pure

eval ast@(LList (LSymbol "+") _) = fail InvalidNumberOfArgumentsError $ "operator '+ in '" <> show ast

eval ast@(LList (LSymbol "-") (Cons x (Cons y Nil))) = eval2Nat (-.) x y >>= maybe (fail InvalidArgumentsError $ "operator '- in '" <> show ast) pure

eval ast@(LList (LSymbol "-") _) = fail InvalidNumberOfArgumentsError $ "operator '- in '" <> show ast

eval ast@(LList (LSymbol "*") (Cons x (Cons y Nil))) = eval2Nat (*) x y >>= maybe (fail InvalidArgumentsError $ "operator '* in '" <> show ast) pure

eval ast@(LList (LSymbol "*") _) = fail InvalidNumberOfArgumentsError $ "operator '* in '" <> show ast

eval ast@(LList (LSymbol "div") (Cons x (Cons y Nil))) = eval2NatPartial (/.) x y >>= maybe (fail InvalidArgumentsError $ "operator 'div in '" <> show ast) pure

eval ast@(LList (LSymbol "div") _) = fail InvalidNumberOfArgumentsError $ "operator 'div in '" <> show ast

eval ast@(LList (LSymbol "mod") (Cons x (Cons y Nil))) = eval2NatPartial partialMod x y >>= maybe (fail InvalidArgumentsError $ "operator 'mod in '" <> show ast) pure

eval ast@(LList (LSymbol "mod") _) = fail InvalidNumberOfArgumentsError $ "operator 'mod in '" <> show ast

eval ast@(LList (LSymbol "<") (Cons x (Cons y Nil))) = eval2NatBool (<) x y >>= maybe (fail InvalidArgumentsError $ "operator '< in '" <> show ast) pure

eval ast@(LList (LSymbol "<") _) = fail InvalidNumberOfArgumentsError $ "operator '< in '" <> show ast

eval ast@(LList (LSymbol ">") (Cons x (Cons y Nil))) = eval2NatBool (>) x y >>= maybe (fail InvalidArgumentsError $ "operator '> in '" <> show ast) pure

eval ast@(LList (LSymbol ">") _) = fail InvalidNumberOfArgumentsError $ "operator '> in '" <> show ast

-- Condition
eval (LList (LSymbol "if") (Cons cond (Cons _then (Cons _else Nil)))) = ifM (toBool <$> eval cond) (eval _then) (eval _else)

eval ast@(LList (LSymbol "if") _) = fail InvalidNumberOfArgumentsError $ "condition 'if in '" <> show ast

-- Applications
eval ast@(LList f (Cons arg Nil)) =
  eval f
    >>= \v -> case v of
        VClosure ident body env -> runEvaluator <$> (insert ident <$> eval arg <*> get) <@> eval body >>= fromEither
        self@(VRecClosure funName ident body env) -> runEvaluator <$> (insert funName self <$> (insert ident <$> eval arg <*> get)) <@> eval body >>= fromEither
        _ -> fail InvalidApplicationError $ "in '" <> show ast

eval ast@(LList _ _) = fail InvalidArgumentsError $ " in " <> show ast

eval2Nat :: (Natural -> Natural -> Natural) -> Lisp -> Lisp -> Evaluator Env (Maybe Value)
eval2Nat f x y = partialLift2Nat f <$> eval x <*> eval y

eval2NatPartial :: (Natural -> Natural -> Maybe Natural) -> Lisp -> Lisp -> Evaluator Env (Maybe Value)
eval2NatPartial f x y = partialLift2NatPartial f <$> eval x <*> eval y

eval2NatBool :: (Natural -> Natural -> Boolean) -> Lisp -> Lisp -> Evaluator Env (Maybe Value)
eval2NatBool f x y = partialLift2NatBool f <$> eval x <*> eval y
