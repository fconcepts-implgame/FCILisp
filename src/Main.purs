module Main where

import Prelude hiding (Ordering(..))
import Data.Foldable (for_)
import Data.List (List(..), (:), (..))
import Data.Natural (fromInt)
import Effect (Effect)
import Effect.Console (log)
import FciLisp.Core.Ast (Lisp(..))
import FciLisp.Core.Evaluator.Class (runEvaluator)
import FciLisp.Core.Evaluator.Expressions (eval, initEnv)

main :: Effect Unit
main =
  let
    nil = LNil

    t = LT

    n i = LNat $ fromInt i

    s str = LSymbol str

    op name args = LList (LSymbol name) args

    cons x y = op "cons" $ x : y : Nil

    head x = op "head" $ x : Nil

    tail x = op "tail" $ x : Nil

    add x y = op "+" $ x : y : Nil

    sub x y = op "-" $ x : y : Nil

    lt x y = op "<" $ x : y : Nil

    gt x y = op ">" $ x : y : Nil

    fun x body = op "fun" $ (s x) : body : Nil

    fix f x body = op "fix" $ (s f) : (s x) : body : Nil

    ap f x = LList f $ x : Nil

    ast1 = tail $ cons (n 0) $ cons (n 1) $ cons (n 2) nil

    ast2 = add (n 2) (n 5)

    ast3 = add (n 8) $ sub (n 2) (n 9)

    ast4 = lt ast2 ast3

    ast5 = gt ast2 ast3

    ast6 = fun "n" $ op "+" $ (s "n") : (n 1) : Nil

    ast7 = ap ast6 (n 4)

    fibo =
      fix "f" "n" $ op "if" $ (op "<" $ (s "n") : (n 2) : Nil)
        : (s "n")
        : ( op "+" $ (op "f" $ (op "-" $ (s "n") : (n 1) : Nil) : Nil)
              : (op "f" $ (op "-" $ (s "n") : (n 2) : Nil) : Nil)
              : Nil
          )
        : Nil

    callFibo i = ap fibo (n i)
  in
    do
      log "Hello sailor!"
      log $ "AST: " <> show ast1
      log $ "Eval: " <> show (runEvaluator initEnv $ eval ast1)
      log $ "AST: " <> show ast2
      log $ "Eval: " <> show (runEvaluator initEnv $ eval ast2)
      log $ "AST: " <> show ast3
      log $ "Eval: " <> show (runEvaluator initEnv $ eval ast3)
      log $ "AST: " <> show ast4
      log $ "Eval: " <> show (runEvaluator initEnv $ eval ast4)
      log $ "AST: " <> show ast5
      log $ "Eval: " <> show (runEvaluator initEnv $ eval ast5)
      log $ "AST: " <> show ast6
      log $ "Eval: " <> show (runEvaluator initEnv $ eval ast6)
      log $ "AST: " <> show ast7
      log $ "Eval: " <> show (runEvaluator initEnv $ eval ast7)
      log $ "fibo: " <> show fibo
      for_ (0 .. 10) \i -> do
        log $ "fibo " <> show i <> ": " <> show (runEvaluator initEnv $ eval $ callFibo i)
