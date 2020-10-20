module DFDB.Transaction
  ( Transaction'(Transaction), Transaction, runTransaction
  ) where

import ClassyPrelude
import Control.Monad.Except (Except, runExcept)
import Control.Monad.State (MonadState, StateT, get, put, runStateT)

import qualified DFDB.Types

newtype Transaction' s e a = Transaction (StateT s (Except e) a)
  deriving (Functor, Applicative, Monad)

type Transaction = Transaction' DFDB.Types.Database DFDB.Types.StatementFailureCode

runTransaction :: (MonadState s m) => Transaction' s e a -> m (Either e a)
runTransaction (Transaction mx) = do
  pre <- get
  let result = runExcept $ runStateT mx pre
  traverse (\ (out, post) -> put post >> pure out) result
