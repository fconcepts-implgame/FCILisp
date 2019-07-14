module Main where

import Prelude hiding (Ordering(..))
import Data.List (List(..), (:))
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
    op name args = LList (LSymbol name) args
    cons x y = op "cons" $ x:y:Nil
    head x = op "head" $ x:Nil
    tail x = op "tail" $ x:Nil
    add x y = op "+" $ x:y:Nil
    sub x y = op "-" $ x:y:Nil
    lt x y = op "<" $ x:y:Nil
    gt x y = op ">" $ x:y:Nil
    fun x body = op "fun" $ (LSymbol x):body:Nil
    ap f x = LList f $ x:Nil
    ast1 = tail $ cons (n 0) $ cons (n 1) $ cons (n 2) nil
    ast2 = add (n 2) (n 5)
    ast3 = add (n 8) $ sub (n 2) (n 9)
    ast4 = lt ast2 ast3
    ast5 = gt ast2 ast3
    ast6 = fun "n" $ op "+" $ (LSymbol "n"):(n 1):Nil
    ast7 = ap ast6 (n 4)
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

{-
-- fibonatti
((lambda ()
  (def fibo (lambda (n)
    (cond ((< n 3) 1)
           (t (+ (fibo (- n 1)) (fibo (- n 2)))))))
 (fibo 4)))
-}