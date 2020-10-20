module DFDB.Database where

import ClassyPrelude
import Control.Lens (_Just, assign, at, each, modifying, over, toListOf, use, view)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.State (MonadState)
import Data.Aeson (encode)
import Data.List ((!!), elemIndex)

import qualified DFDB.Tree
import qualified DFDB.Types

emptyDatabase :: DFDB.Types.Database
emptyDatabase = DFDB.Types.Database mempty mempty

getTable :: MonadState DFDB.Types.Database m => DFDB.Types.TableName -> m (Maybe DFDB.Types.Table)
getTable tableName = use (DFDB.Types.databaseTables . at tableName)

getTableOrFail :: (MonadError DFDB.Types.StatementFailureCode m, MonadState DFDB.Types.Database m) => DFDB.Types.TableName -> m DFDB.Types.Table
getTableOrFail tableName = maybe (throwError (DFDB.Types.StatementFailureCodeSyntaxError "Table does not exist")) pure
  =<< getTable tableName

getIndex :: MonadState DFDB.Types.Database m => DFDB.Types.IndexName -> m (Maybe DFDB.Types.Index)
getIndex indexName = use (DFDB.Types.databaseIndices . at indexName)

getIndexOrFail :: (MonadError DFDB.Types.StatementFailureCode m, MonadState DFDB.Types.Database m) => DFDB.Types.IndexName -> m DFDB.Types.Index
getIndexOrFail indexName = maybe (throwError (DFDB.Types.StatementFailureCodeSyntaxError "Index does not exist")) pure
  =<< getIndex indexName

runInner :: Monad m => ExceptT DFDB.Types.StatementFailureCode m DFDB.Types.Output -> m DFDB.Types.StatementResult
runInner mx = runExceptT mx >>= \ case
  Left code -> pure $ DFDB.Types.StatementResultFailure code
  Right output -> pure $ DFDB.Types.StatementResultSuccess output

execute :: MonadState DFDB.Types.Database m => DFDB.Types.Statement -> m DFDB.Types.StatementResult
execute = \ case
  -- execute a select
  DFDB.Types.StatementSelect cols tableName -> runInner $ do
    table <- getTableOrFail tableName
    columnIndices <- for cols $ \ col -> case elemIndex col (toListOf (DFDB.Types.tableDefinition . each . DFDB.Types.columnDefinitionName) table) of
      Nothing -> throwError $ DFDB.Types.StatementFailureCodeSyntaxError $ "Column " <> DFDB.Types.unColumnName col <> " does not exist in " <> DFDB.Types.unTableName tableName
      Just c -> pure c
    let rows = flip DFDB.Tree.map (view DFDB.Types.tableRows table) $ \ (DFDB.Types.Row atoms) -> flip map columnIndices $ \ columnIndex -> atoms !! columnIndex
    pure . DFDB.Types.Output . unlines . map (decodeUtf8 . toStrict . encode) . DFDB.Tree.toList $ rows

  -- execute an insert
  DFDB.Types.StatementInsert row tableName -> runInner $ do
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
        assign (DFDB.Types.databaseTables . at tableName . _Just) (over DFDB.Types.tableRows (DFDB.Tree.insert row) table)
        pure $ DFDB.Types.Output "INSERT 1"

  -- execute a create table
  DFDB.Types.StatementCreate tableName cols -> runInner $ do
    use (DFDB.Types.databaseTables . at tableName) >>= \ case
      Nothing -> do
        modifying DFDB.Types.databaseTables (insertMap tableName $ DFDB.Types.Table tableName cols DFDB.Tree.empty)
        pure $ DFDB.Types.Output "CREATE TABLE"
      Just _ -> throwError $ DFDB.Types.StatementFailureCodeSyntaxError "Table already exists"

  -- execute a create index
  DFDB.Types.StatementCreateIndex indexName tableName cols -> runInner $ do
    table <- use (DFDB.Types.databaseTables . at tableName) >>= \ case
      Nothing -> throwError $ DFDB.Types.StatementFailureCodeSyntaxError "Table does not exist"
      Just t -> pure t
    let extraColumns =
          intercalate ", " . map DFDB.Types.unColumnName . setToList . difference (setFromList cols)
            . asSet . setFromList . toListOf (DFDB.Types.tableDefinition . each . DFDB.Types.columnDefinitionName)
            $ table
    unless (null extraColumns) $ throwError $ DFDB.Types.StatementFailureCodeSyntaxError $ "Columns " <> extraColumns <> " not in table"
    use (DFDB.Types.databaseIndices . at indexName) >>= \ case
      Nothing -> do
        modifying DFDB.Types.databaseIndices (insertMap indexName $ DFDB.Types.Index indexName tableName cols)
        pure $ DFDB.Types.Output "CREATE INDEX"
      Just _ -> throwError $ DFDB.Types.StatementFailureCodeSyntaxError "Index already exists"

  -- execute a table drop
  DFDB.Types.StatementDrop tableName -> runInner $ do
    void $ getTableOrFail tableName
    modifying DFDB.Types.databaseTables (deleteMap tableName)
    pure $ DFDB.Types.Output "DROP TABLE"

  -- execute an index drop
  DFDB.Types.StatementDropIndex indexName -> runInner $ do
    void $ getIndexOrFail indexName
    modifying DFDB.Types.databaseIndices (deleteMap indexName)
    pure $ DFDB.Types.Output "DROP INDEX"
