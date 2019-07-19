module FciLisp.Core.Evaluator
  ( eval
  ) where

import Prelude hiding (Ordering(..))
import Data.List (List(..))
import Data.Map (lookup, insert)
import Data.Maybe (Maybe, maybe)
import Data.Natural (Natural, (-.), (/.), partialMod)
import FciLisp.Core.Evaluator.Class (ErrorType(..), Evaluator, fail, get, gets, runEvaluator, fromEither)
import FciLisp.Core.Interfaces.Ast (Lisp(..))
import FciLisp.Core.Interfaces.Value (Value(..), eqAsValue, isAtom, fromBool, toBool, partialHead, partialTail, partialLift2Nat, partialLift2NatPartial, partialLift2NatBool, Env)

eval :: Lisp -> Evaluator Env Value
-- Literals and Constructors
eval LNil = pure VNil

eval LT = pure VT

eval (LNat n) = pure $ VNat n

eval (LSymbol ident) = gets (lookup ident) >>= maybe (fail UnboundedVariableError $ "variable '" <> ident) pure

eval (LList (LSymbol "fun") (Cons (LSymbol ident) (Cons body Nil))) = VClosure ident body <$> get

eval (LList (LSymbol "fun") _) = fail InvalidNumberOfArgumentsError "in 'fun"

eval (LList (LSymbol "recur") (Cons (LSymbol funName) (Cons (LSymbol ident) (Cons body Nil)))) = VRecClosure funName ident body <$> get

eval (LList (LSymbol "recur") _) = fail InvalidNumberOfArgumentsError "in 'recur"

eval (LList (LSymbol "cons") (Cons x (Cons y Nil))) = VPair <$> eval x <*> eval y

eval (LList (LSymbol "cons") _) = fail InvalidNumberOfArgumentsError "in 'cons"

-- Predicatres
eval (LList (LSymbol "atom?") (Cons x Nil)) = isAtom <$> eval x <#> fromBool

eval (LList (LSymbol "atom?") _) = fail InvalidNumberOfArgumentsError "in 'atom?"

eval (LList (LSymbol "eq?") (Cons x (Cons y Nil))) = eqAsValue <$> eval x <*> eval y <#> fromBool

eval (LList (LSymbol "eq?") _) = fail InvalidNumberOfArgumentsError "in 'eq?"

-- Operators
eval (LList (LSymbol "head") (Cons x Nil)) = partialHead <$> eval x >>= maybe (fail InvalidArgumentsError "in 'head") pure

eval (LList (LSymbol "head") _) = fail InvalidNumberOfArgumentsError "in 'head"

eval (LList (LSymbol "tail") (Cons x Nil)) = partialTail <$> eval x >>= maybe (fail InvalidArgumentsError "in 'tail") pure

eval (LList (LSymbol "tail") _) = fail InvalidNumberOfArgumentsError "in 'tail"

eval (LList (LSymbol "+") (Cons x (Cons y Nil))) = eval2Nat (+) x y >>= maybe (fail InvalidArgumentsError "in '+") pure

eval (LList (LSymbol "+") _) = fail InvalidNumberOfArgumentsError "in '+"

eval (LList (LSymbol "-") (Cons x (Cons y Nil))) = eval2Nat (-.) x y >>= maybe (fail InvalidArgumentsError "in '-") pure

eval (LList (LSymbol "-") _) = fail InvalidNumberOfArgumentsError "in '-"

eval (LList (LSymbol "*") (Cons x (Cons y Nil))) = eval2Nat (*) x y >>= maybe (fail InvalidArgumentsError "in '*") pure

eval (LList (LSymbol "*") _) = fail InvalidNumberOfArgumentsError "in '*"

eval (LList (LSymbol "div") (Cons x (Cons y Nil))) = eval2NatPartial (/.) x y >>= maybe (fail InvalidArgumentsError "in 'div") pure

eval (LList (LSymbol "div") _) = fail InvalidNumberOfArgumentsError "in 'div"

eval (LList (LSymbol "mod") (Cons x (Cons y Nil))) = eval2NatPartial partialMod x y >>= maybe (fail InvalidArgumentsError "in 'mod") pure

eval (LList (LSymbol "mod") _) = fail InvalidNumberOfArgumentsError "in 'mod"

eval (LList (LSymbol "<") (Cons x (Cons y Nil))) = eval2NatBool (<) x y >>= maybe (fail InvalidArgumentsError "in '<") pure

eval (LList (LSymbol ">") (Cons x (Cons y Nil))) = eval2NatBool (>) x y >>= maybe (fail InvalidArgumentsError "in '>") pure

-- Condition
eval (LList (LSymbol "if") (Cons cond (Cons _then (Cons _else Nil)))) = ifM (toBool <$> eval cond) (eval _then) (eval _else)

-- Applications
eval (LList f (Cons arg Nil)) =
  eval f
    >>= \v -> case v of
        VClosure ident body env -> runEvaluator <$> (insert ident <$> eval arg <*> get) <@> eval body >>= fromEither
        self@(VRecClosure funName ident body env) -> runEvaluator <$> (insert funName self <$> (insert ident <$> eval arg <*> get)) <@> eval body >>= fromEither
        _ -> fail InvalidApplicationError ""

eval _ = fail SyntaxError $ ""

eval2Nat :: (Natural -> Natural -> Natural) -> Lisp -> Lisp -> Evaluator Env (Maybe Value)
eval2Nat f x y = partialLift2Nat f <$> eval x <*> eval y

eval2NatPartial :: (Natural -> Natural -> Maybe Natural) -> Lisp -> Lisp -> Evaluator Env (Maybe Value)
eval2NatPartial f x y = partialLift2NatPartial f <$> eval x <*> eval y

eval2NatBool :: (Natural -> Natural -> Boolean) -> Lisp -> Lisp -> Evaluator Env (Maybe Value)
eval2NatBool f x y = partialLift2NatBool f <$> eval x <*> eval y
