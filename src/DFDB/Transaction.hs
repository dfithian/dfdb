module DFDB.Transaction
  ( Transaction(Transaction), runTransaction
  ) where

import ClassyPrelude
import Control.Monad.Except (Except, runExcept)
import Control.Monad.State (MonadState, StateT, get, runStateT, put)

import qualified DFDB.Types

newtype Transaction a = Transaction (StateT DFDB.Types.Database (Except DFDB.Types.StatementFailureCode) a)
  deriving (Functor, Applicative, Monad)

runTransaction :: (MonadState DFDB.Types.Database m) => Transaction a -> m (Either DFDB.Types.StatementFailureCode a)
runTransaction (Transaction mx) = do
  pre <- get
  let result = runExcept $ runStateT mx pre
  traverse (\ (out, post) -> put post >> pure out) result
