module DFDB.Transaction
  ( Transaction'(Transaction), Transaction, runTransaction
  , MonadDatabase
  ) where

import ClassyPrelude
import Control.Monad.Except (Except, MonadError, runExcept)
import Control.Monad.State (MonadState, StateT, get, put, runStateT)

import qualified DFDB.Types

newtype Transaction' s e a = Transaction (StateT s (Except e) a)
  deriving (Functor, Applicative, Monad)

type Transaction = Transaction' DFDB.Types.Database DFDB.Types.StatementFailureCode

type MonadDatabase m = (MonadState DFDB.Types.Database m, MonadError DFDB.Types.StatementFailureCode m)

runTransaction :: (MonadState s m) => Transaction' s e a -> m (Either e a)
runTransaction (Transaction mx) = do
  pre <- get
  let result = runExcept $ runStateT mx pre
  traverse (\ (out, post) -> put post >> pure out) result
