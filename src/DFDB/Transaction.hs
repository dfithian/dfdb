module DFDB.Transaction
  ( Transaction(Transaction), runTransaction
  , MonadDatabase
  ) where

import ClassyPrelude
import Control.Lens (_1, _2, _Just, assign, view)
import Control.Monad.Except (Except, MonadError, runExcept)
import Control.Monad.State (MonadState, StateT, get, put, runStateT)

import qualified DFDB.Types

newtype Transaction a = Transaction (StateT (DFDB.Types.Database, Maybe DFDB.Types.TransactionStatus) (Except DFDB.Types.StatementFailureCode) a)
  deriving (Functor, Applicative, Monad)

type MonadDatabase m = (MonadState (DFDB.Types.Database, Maybe DFDB.Types.TransactionStatus) m, MonadError DFDB.Types.StatementFailureCode m)

runAutocommit :: (MonadState DFDB.Types.TransactionalDatabase m) => DFDB.Types.Database -> Transaction a -> m (Either DFDB.Types.StatementFailureCode a)
runAutocommit pre (Transaction mx) = case runExcept $ runStateT mx (pre, Nothing) of
  Left err -> pure $ Left err
  Right (out, (post, postStatusMay)) -> do
    case postStatusMay of
      Nothing -> assign DFDB.Types.transactionalDatabaseLastSavepoint post
      Just postStatus -> do
        put DFDB.Types.TransactionalDatabase
          { _transactionalDatabaseLastSavepoint = pre
          , _transactionalDatabaseInner = Just (postStatus, post)
          }
    pure $ Right out

runInner :: (MonadState DFDB.Types.TransactionalDatabase m) => DFDB.Types.TransactionStatus -> DFDB.Types.Database -> Transaction a -> m (Either DFDB.Types.StatementFailureCode a)
runInner preStatus pre (Transaction mx) = case runExcept $ runStateT mx (pre, Just preStatus) of
  Left err -> do
    assign (DFDB.Types.transactionalDatabaseInner . _Just . _1) DFDB.Types.TransactionStatusAborted
    pure $ Left err
  Right (out, (post, postStatusMay)) -> do
    case postStatusMay of
      Nothing -> put DFDB.Types.TransactionalDatabase
        { _transactionalDatabaseLastSavepoint = post
        , _transactionalDatabaseInner = Nothing
        }
      Just DFDB.Types.TransactionStatusBegin -> assign (DFDB.Types.transactionalDatabaseInner . _Just . _2) post
      Just DFDB.Types.TransactionStatusAborted -> pure ()
      Just DFDB.Types.TransactionStatusCommit -> put DFDB.Types.TransactionalDatabase
        { _transactionalDatabaseLastSavepoint = post
        , _transactionalDatabaseInner = Nothing
        }
      Just DFDB.Types.TransactionStatusRollback -> assign DFDB.Types.transactionalDatabaseInner Nothing
    pure $ Right out

runTransaction :: (MonadState DFDB.Types.TransactionalDatabase m) => Transaction a -> m (Either DFDB.Types.StatementFailureCode a)
runTransaction tx = do
  pre <- get
  case view DFDB.Types.transactionalDatabaseInner pre of
    Nothing -> runAutocommit (view DFDB.Types.transactionalDatabaseLastSavepoint pre) tx
    Just (status, innerPre) -> runInner status innerPre tx
