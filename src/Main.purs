module Main where

import Prelude
import Data.Show (show)
import Effect (Effect)
import Effect.Console (log)
import FciLisp.Core.Ast (Expression(..))
import FciLisp.Core.Evaluator.Class (runEvaluator)
import FciLisp.Core.Evaluator.Expressions (evaluator, initialEnvironment)

main :: Effect Unit
main =
  let
    nil = List []

    zero = nil

    one = List [ Atom "cons", zero, nil ]

    two = List [ Atom "cons", zero, List [ Atom "cons", one, nil ] ]

    ast1 = List [ Atom "cdr", List [ Atom "cons", List [], List [ Atom "cons", List [], List [] ] ] ]

    ast2 = List [ List [], List [ Atom "cons", List [], List [] ] ]
  in
    do
      log "Hello sailor!"
      log $ "AST1: " <> show ast1
      log $ "eval: " <> (show $ runEvaluator initialEnvironment $ evaluator ast1)
      log $ show zero
      log $ show one
      log $ show two

{-
-- fibonatti
((lambda ()
  (def fibo (lambda (n)
    (cond ((< n 3) 1)
           (t (+ (fibo (- n 1)) (fibo (- n 2)))))))
 (fibo 4)))
-}