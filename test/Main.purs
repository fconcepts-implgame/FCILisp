module Test.Main where

import Prelude hiding (Ordering(..))
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Identity (Identity)
import Data.List (List(..), (:))
import Data.Natural (fromInt)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Text.Parsing.Parser (runParser)
import FciLisp.Core.Interfaces.Ast (Lisp(..))
import FciLisp.Core.Interfaces.Value (Value(..), initEnv)
import FciLisp.Core.Evaluator.Class (runEvaluator)
import FciLisp.Core.Evaluator (eval)
import FciLisp.Core.Parser (expression)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "fci-lisp" do
          testParsers
          testEvaluators

testParsers :: SpecT Aff Unit Identity Unit
testParsers =
  let
    validTests =
      [ Tuple "nil" LNil
      , Tuple "()" LNil
      , Tuple "t" LT
      , Tuple "32" $ LNat $ fromInt 32
      , Tuple "<" $ LSymbol "<"
      , Tuple "1a" $ LSymbol "1a"
      , Tuple "(atom)" $ LList (LSymbol "atom") Nil
      , Tuple "(cons 3 1a)" $ LList (LSymbol "cons") $ (LNat $ fromInt 3) : (LSymbol "1a") : Nil
      , Tuple "(a   )" $ LList (LSymbol "a") Nil
      , Tuple "(a (a b) ( ) c)" $ LList (LSymbol "a") $ (LList (LSymbol "a") $ (LSymbol "b") : Nil) : LNil : (LSymbol "c") : Nil
      , Tuple "( a   (  b )   c )" $ LList (LSymbol "a") $ (LList (LSymbol "b") Nil) : (LSymbol "c") : Nil
      , Tuple
          """(recur f n (if (< n 2)
                                          n
                                          (+ (f (- n 1))
                                             (f (- n 2))
                                          )
                                        )
                               )"""
          $ LList (LSymbol "recur")
          $ (LSymbol "f")
          : (LSymbol "n")
          : ( LList (LSymbol "if")
                $ ( LList (LSymbol "<") $ (LSymbol "n")
                      : (LNat $ fromInt 2)
                      : Nil
                  )
                : (LSymbol "n")
                : ( LList (LSymbol "+")
                      $ ( LList (LSymbol "f")
                            $ ( LList (LSymbol "-") $ (LSymbol "n")
                                  : (LNat $ fromInt 1)
                                  : Nil
                              )
                            : Nil
                        )
                      : ( LList (LSymbol "f")
                            $ ( LList (LSymbol "-") $ (LSymbol "n")
                                  : (LNat $ fromInt 2)
                                  : Nil
                              )
                            : Nil
                        )
                      : Nil
                  )
                : Nil
            )
          : Nil
      ]

    invalidTests =
      [ Tuple "-a(" \result -> case result of
          Left _ -> true
          _ -> false
      ]

    testValid code result =
      it code do
        (runParser code expression) `shouldEqual` (pure result)

    testInvalid code satisfy =
      it code do
        (runParser code expression) `shouldSatisfy` satisfy
  in
    describe "FciLisp" do
      describe "Core" do
        describe "Parser" do
          for_ validTests \(Tuple code result) -> do
            testValid code result
          for_ invalidTests \(Tuple code satisfy) -> do
            testInvalid code satisfy

testEvaluators :: SpecT Aff Unit Identity Unit
testEvaluators =
  let
    nil = LNil

    t = LT

    nat i = LNat $ fromInt i

    sym str = LSymbol str

    op name args = LList (sym name) args

    op1 name x = op name $ x : Nil

    op2 name x y = op name $ x : y : Nil

    op3 name x y z = op name $ x : y : z : Nil

    fun x body = op2 "fun" (sym x) body

    recur f x body = op3 "recur" (sym f) (sym x) body

    apply f x = LList f $ x : Nil

    num1 = op2 "+" (nat 2) (nat 5)

    num2 = op2 "-" (nat 2) (nat 9)

    num3 = op2 "+" (nat 8) num2

    fBody1 = op2 "+" (sym "n") (nat 1)

    f1 = fun "n" fBody1

    factBody = op3 "if" (op2 "eq?" (sym "n") (nat 0)) (nat 1) (op2 "*" (sym "n") (op1 "f" (op2 "-" (sym "n") (nat 1))))

    fact = recur "f" "n" factBody

    fibo = recur "fibo" "n" $ op3 "if" (op2 "<" (sym "n") (nat 2)) (sym "n") (op2 "+" (op1 "fibo" (op2 "-" (sym "n") (nat 1))) (op1 "fibo" (op2 "-" (sym "n") (nat 2))))

    validTests =
      [ Tuple (op1 "tail" $ op2 "cons" (nat 1) (op2 "cons" (nat 2) nil))
          $ VPair (VNat $ fromInt 2) VNil
      , Tuple (op2 "eq?" nil nil)
          VT
      , Tuple num1
          $ VNat
          $ fromInt 7
      , Tuple num2
          $ VNat
          $ fromInt 0
      , Tuple num3
          $ VNat
          $ fromInt 8
      , Tuple (op2 "<" num1 num3)
          $ VT
      , Tuple (op2 ">" num1 num3)
          $ VNil
      , Tuple f1
          $ VClosure "n" fBody1 initEnv
      , Tuple (apply f1 (nat 4))
          $ VNat
          $ fromInt 5
      , Tuple fact
          $ VRecClosure "f" "n" factBody initEnv
      , Tuple (apply fact (nat 1))
          $ VNat
          $ fromInt 1
      , Tuple (apply fact (nat 5))
          $ VNat
          $ fromInt 120
      , Tuple (apply fibo (nat 1))
          $ VNat
          $ fromInt 1
      , Tuple (apply fibo (nat 2))
          $ VNat
          $ fromInt 1
      , Tuple (apply fibo (nat 3))
          $ VNat
          $ fromInt 2
      , Tuple (apply fibo (nat 4))
          $ VNat
          $ fromInt 3
      , Tuple (apply fibo (nat 5))
          $ VNat
          $ fromInt 5
      , Tuple (apply fibo (nat 6))
          $ VNat
          $ fromInt 8
      , Tuple (apply fibo (nat 7))
          $ VNat
          $ fromInt 13
      , Tuple (apply fibo (nat 8))
          $ VNat
          $ fromInt 21
      , Tuple (apply fibo (nat 9))
          $ VNat
          $ fromInt 34
      , Tuple (apply fibo (nat 10))
          $ VNat
          $ fromInt 55
      ]

    testValid ast result =
      it (show ast) do
        (runEvaluator initEnv $ eval ast) `shouldEqual` (pure result)
  in
    describe "FciLisp" do
      describe "Core" do
        describe "Evaluator" do
          for_ validTests \(Tuple ast result) -> do
            testValid ast result
