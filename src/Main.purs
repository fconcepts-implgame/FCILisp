module Main where

import Prelude hiding (Ordering(..))
import Data.Foldable (for_)
import Data.List (List(..), (:), (..))
import Data.Natural (fromInt)
import Effect (Effect)
import Effect.Console (log)
import Text.Parsing.Parser (runParser)
import FciLisp.Core.Ast (Lisp(..))
import FciLisp.Core.Evaluator.Class (runEvaluator)
import FciLisp.Core.Evaluator.Expressions (eval, initEnv)
import FCILisp.Core.Parser (expression)

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

    code1 = "nil"

    code2 = "()"

    code3 = "t"

    code4 = "32"

    code5 = "<"

    code6 = "1a"

    code7 = "(atom)"

    code8 = "-(a"

    code9 = "(cons 3 1a)"

    fiboCode =
      """(fix f n (if (< n 2)
                             n
                             (+ (f (- n 1))
                                (f (- n 2))
                             )
                           )
                  )"""
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
      log $ "Code1: " <> show code1
      log $ "Parse: " <> show (runParser code1 expression)
      log $ "Code2: " <> show code2
      log $ "Parse: " <> show (runParser code2 expression)
      log $ "Code3: " <> show code3
      log $ "Parse: " <> show (runParser code3 expression)
      log $ "Code4: " <> show code4
      log $ "Parse: " <> show (runParser code4 expression)
      log $ "Code5: " <> show code5
      log $ "Parse: " <> show (runParser code5 expression)
      log $ "Code6: " <> show code6
      log $ "Parse: " <> show (runParser code6 expression)
      log $ "Code7: " <> show code7
      log $ "Parse: " <> show (runParser code7 expression)
      log $ "Code8: " <> show code8
      log $ "Parse: " <> show (runParser code8 expression)
      log $ "Code9: " <> show code9
      log $ "Parse: " <> show (runParser code9 expression)
      log $ "Parse: " <> show (runParser "a" expression)
      log $ "Parse: " <> show (runParser "(a)" expression)
      log $ "Parse: " <> show (runParser "(a )" expression)
      log $ "Parse: " <> show (runParser "(())" expression)
      log $ "Parse: " <> show (runParser "(a (a b) (  ) c)" expression)
      log $ "Parse: " <> show (runParser "( a   (  b )   c )" expression)
      log $ "FiboCode: " <> show fiboCode
      log $ "FiboParse: " <> show (runParser fiboCode expression)
      log $ "FiboEval: " <> show ((runEvaluator initEnv) <<< eval <$> runParser fiboCode expression)
      for_ (0 .. 10) \i ->
        let
          fiboApplyCode = "(" <> fiboCode <> " " <> show i <> ")"
        in
          do
            log $ "fibo " <> show i <> ": " <> show ((runEvaluator initEnv) <<< eval <$> runParser fiboApplyCode expression)
