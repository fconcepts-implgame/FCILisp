module FciLisp.Core.Evaluator.Class
  ( RuntimeError(..)
  , runtimeErrorType
  , runtimeErrorMessage
  , ErrorType(..)
  , fail
  , EvaluateState(..)
  , get
  , gets
  , modify_
  , EvaluatorT(..)
  , runEvaluatorT
  , Evaluator
  , runEvaluator
  , fromEither
  ) where

import Prelude
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (class MonadError, ExceptT, runExceptT)
import Control.Monad.State (class MonadState, StateT, evalStateT)
import Control.Monad.State as MS
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Either (Either, either)
import Data.Identity (Identity)
import Data.Newtype (class Newtype, unwrap)

data RuntimeError
  = RuntimeError ErrorType String

runtimeErrorType :: RuntimeError -> ErrorType
runtimeErrorType (RuntimeError etype _) = etype

runtimeErrorMessage :: RuntimeError -> String
runtimeErrorMessage (RuntimeError _ msg) = msg

instance showRuntimeError :: Show RuntimeError where
  show (RuntimeError etype msg) = "(RuntimeError " <> show etype <> ": " <> msg <> ")"

derive instance eqRuntimeError :: Eq RuntimeError

data ErrorType
  = InvalidArgumentsError
  | UnboundedVariableError
  | InvalidNumberOfArgumentsError
  | InvalidApplicationError

fail :: forall m a. MonadThrow RuntimeError m => ErrorType -> String -> m a
fail etype msg = throwError $ RuntimeError etype msg

instance showErrorType :: Show ErrorType where
  show InvalidArgumentsError = "InvalidArgumentsError"
  show UnboundedVariableError = "UnboundedVariableError"
  show InvalidNumberOfArgumentsError = "InvalidNumberOfArgumentsError"
  show InvalidApplicationError = "InvalidApplicationError"

derive instance eqErrorType :: Eq ErrorType

data EvaluateState s
  = EvaluateState s

get :: forall m s. MonadState (EvaluateState s) m => m s
get = MS.gets (\(EvaluateState s) -> s)

gets :: forall s m a. MonadState (EvaluateState s) m => (s -> a) -> m a
gets f = MS.gets (\(EvaluateState s) -> f s)

modify_ :: forall s m. MonadState (EvaluateState s) m => (s -> s) -> m Unit
modify_ f = MS.modify_ (\(EvaluateState s) -> EvaluateState $ f s)

newtype EvaluatorT s m a
  = EvaluatorT (StateT (EvaluateState s) (ExceptT RuntimeError m) a)

derive instance newtypeEvaluatorT :: Newtype (EvaluatorT s m a) _

derive newtype instance fanctorEvaluatorT :: Functor m => Functor (EvaluatorT s m)

derive newtype instance applyEvaluatorT :: Monad m => Apply (EvaluatorT s m)

derive newtype instance applicativeEvaluatorT :: Monad m => Applicative (EvaluatorT s m)

derive newtype instance bindEvaluatorT :: Monad m => Bind (EvaluatorT s m)

derive newtype instance monadEvaluatorT :: Monad m => Monad (EvaluatorT s m)

derive newtype instance monadStateEvaluatorT :: Monad m => MonadState (EvaluateState s) (EvaluatorT s m)

derive newtype instance monadThrowEvaluatorT :: Monad m => MonadThrow RuntimeError (EvaluatorT s m)

derive newtype instance monadErrorEvaluatorT :: Monad m => MonadError RuntimeError (EvaluatorT s m)

instance monadTransEvaluatorT :: MonadTrans (EvaluatorT s) where
  lift = EvaluatorT <<< lift <<< lift

runEvaluatorT :: forall s m a. Monad m => s -> EvaluatorT s m a -> m (Either RuntimeError a)
runEvaluatorT env eval = runExceptT (evalStateT (unwrap eval) (EvaluateState env))

type Evaluator s
  = EvaluatorT s Identity

runEvaluator :: forall s a. s -> Evaluator s a -> Either RuntimeError a
runEvaluator env = unwrap <<< runEvaluatorT env

fromEither :: forall s a. Either RuntimeError a -> Evaluator s a
fromEither = either throwError pure
