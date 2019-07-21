module FciLisp.Repl.Cli where

import Prelude hiding (Ordering(..))
import Data.Either (Either(..), either)
import Data.Options ((:=))
import Data.String.Common (trim)
import Effect (Effect)
import Effect.Console (log, logShow)
import FciLisp.Core.Evaluator (eval)
import FciLisp.Core.Evaluator.Class (RuntimeError, ErrorType(..), fail, runEvaluator)
import FciLisp.Core.Interfaces.Value (Value, initEnv)
import FciLisp.Core.Parser (expression)
import Node.Process (stdin, stdout)
import Node.ReadLine (Interface, close, createInterface, output, prompt, setLineHandler)
import Text.Parsing.Parser (runParser)

main :: Effect Unit
main = do
  rl <- createInterface stdin $ output := stdout
  log "FCILisp REPL"
  log "Type :help for help"
  log ""
  repl rl

repl :: Interface -> Effect Unit
repl rl = do
  prompt rl
  setLineHandler rl $ trim
    >>> \s -> if s == "" then do
        repl rl
      else if s == ":help" then do
        log help
        repl rl
      else if s == ":exit" || s == ":quit" then do
        log "Bye!"
        close rl
      else do
        case run s of
          Right v -> logShow v
          Left e -> logShow e
        repl rl

run :: String -> Either RuntimeError Value
run s = runParser s expression # either (\e -> fail (InvalidSyntaxError e) "") (runEvaluator initEnv <<< eval)

help :: String
help =
  """    Help        :help
    Exit        :exit or :quit"""
