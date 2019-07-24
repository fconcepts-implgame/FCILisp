module FciLisp.Core.Interfaces.RuntimeError
  ( RuntimeError
  , runtimeErrorType
  , runtimeErrorMessage
  , ErrorType(..)
  , fail
  ) where

import Prelude
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Text.Parsing.Parser (ParseError)

data RuntimeError
  = RuntimeError ErrorType String

runtimeErrorType :: RuntimeError -> ErrorType
runtimeErrorType (RuntimeError etype _) = etype

runtimeErrorMessage :: RuntimeError -> String
runtimeErrorMessage (RuntimeError _ msg) = msg

instance showRuntimeError :: Show RuntimeError where
  show (RuntimeError etype msg) = "(RuntimeError " <> show etype <> " " <> show msg <> ")"

derive instance eqRuntimeError :: Eq RuntimeError

data ErrorType
  = InvalidArgumentsError
  | UnboundedVariableError
  | InvalidNumberOfArgumentsError
  | InvalidApplicationError
  | InvalidSyntaxError ParseError

fail :: forall m a. MonadThrow RuntimeError m => ErrorType -> String -> m a
fail etype msg = throwError $ RuntimeError etype msg

instance showErrorType :: Show ErrorType where
  show InvalidArgumentsError = "InvalidArgumentsError"
  show UnboundedVariableError = "UnboundedVariableError"
  show InvalidNumberOfArgumentsError = "InvalidNumberOfArgumentsError"
  show InvalidApplicationError = "InvalidApplicationError"
  show (InvalidSyntaxError e) = "(SyntaxError " <> show e <> ")"

derive instance eqErrorType :: Eq ErrorType
