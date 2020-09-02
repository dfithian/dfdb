module DFDB.Database where

import ClassyPrelude
import Control.Lens (_Just, assign, at, each, modifying, over, toListOf, use, view)
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.State (MonadState)
import Data.Aeson (encode)
import Data.List ((!!), elemIndex)

import qualified DFDB.Types

emptyDatabase :: DFDB.Types.Database
emptyDatabase = DFDB.Types.Database mempty

getTable :: MonadState DFDB.Types.Database m => DFDB.Types.TableName -> m (Maybe DFDB.Types.Table)
getTable tableName = use (DFDB.Types.databaseTables . at tableName)

getTableOrFail :: (MonadError DFDB.Types.StatementFailureCode m, MonadState DFDB.Types.Database m) => DFDB.Types.TableName -> m DFDB.Types.Table
getTableOrFail tableName = maybe (throwError (DFDB.Types.StatementFailureCodeSyntaxError "Table does not exist")) pure
  =<< getTable tableName

execute :: MonadState DFDB.Types.Database m => DFDB.Types.Statement -> m DFDB.Types.StatementResult
execute = \ case
  -- execute a select
  DFDB.Types.StatementSelect cols tableName -> do
    result <- runExceptT $ do
      table <- getTableOrFail tableName
      columnIndices <- for cols $ \ col -> case elemIndex col (toListOf (DFDB.Types.tableDefinition . each . DFDB.Types.columnDefinitionName) table) of
        Nothing -> throwError $ DFDB.Types.StatementFailureCodeSyntaxError $ "Column " <> DFDB.Types.unColumnName col <> " does not exist in " <> DFDB.Types.unTableName tableName
        Just c -> pure c
      let rows = flip map (view DFDB.Types.tableRows table) $ \ (DFDB.Types.Row atoms) -> flip map columnIndices $ \ columnIndex -> atoms !! columnIndex
      pure . DFDB.Types.StatementResultSuccess . DFDB.Types.Output . unlines . map (decodeUtf8 . toStrict . encode) $ rows
    case result of
      Left code -> pure $ DFDB.Types.StatementResultFailure code
      Right x -> pure x

  -- execute an insert
  DFDB.Types.StatementInsert row tableName -> do
    result <- runExceptT $ do
      table <- getTableOrFail tableName
      let columnDefinitions = view DFDB.Types.tableDefinition table
      case length (DFDB.Types.unRow row) == length columnDefinitions of
        False -> throwError $ DFDB.Types.StatementFailureCodeSyntaxError "Wrong number of columns"
        True -> do
          for_ (zip (DFDB.Types.unRow row) columnDefinitions) $ \ (atom, columnDefinition) ->
            let atomType = view DFDB.Types.columnDefinitionType columnDefinition
                column = view DFDB.Types.columnDefinitionName columnDefinition
            in case DFDB.Types.toAtomType atom == atomType of
              True -> pure ()
              False -> throwError $ DFDB.Types.StatementFailureCodeSyntaxError $ "Column " <> DFDB.Types.unColumnName column <> " (" <> tshow atom <> ") is not a " <> tshow atomType
          assign (DFDB.Types.databaseTables . at tableName . _Just) (over DFDB.Types.tableRows (row:) table)
    case result of
      Left code -> pure $ DFDB.Types.StatementResultFailure code
      Right () -> pure $ DFDB.Types.StatementResultSuccess $ DFDB.Types.Output "INSERT 1"

  -- execute a create table
  DFDB.Types.StatementCreate tableName cols -> do
    use (DFDB.Types.databaseTables . at tableName) >>= \ case
      Nothing -> do
        modifying DFDB.Types.databaseTables (insertMap tableName $ DFDB.Types.Table tableName cols [])
        pure $ DFDB.Types.StatementResultSuccess $ DFDB.Types.Output "CREATE TABLE"
      Just _ -> pure $ DFDB.Types.StatementResultFailure $ DFDB.Types.StatementFailureCodeSyntaxError "Table already exists"
