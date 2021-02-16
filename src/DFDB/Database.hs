module DFDB.Database where

import ClassyPrelude hiding (index)
import Control.Lens (_Just, assign, at, each, modifying, over, toListOf, use, view)
import Control.Monad.Except (throwError)
import Data.Aeson (encode)
import Data.List ((!!))
import qualified Data.List as List

import qualified DFDB.Transaction
import qualified DFDB.Tree
import qualified DFDB.Types

emptyDatabase :: DFDB.Types.Database
emptyDatabase = DFDB.Types.Database mempty mempty

getTable :: DFDB.Transaction.MonadDatabase m => DFDB.Types.TableName -> m (Maybe DFDB.Types.Table)
getTable tableName = use (DFDB.Types.databaseTables . at tableName)

getTableOrFail :: DFDB.Transaction.MonadDatabase m => DFDB.Types.TableName -> m DFDB.Types.Table
getTableOrFail tableName = maybe (throwError (DFDB.Types.StatementFailureCodeSyntaxError "Table does not exist")) pure
  =<< getTable tableName

getIndex :: DFDB.Transaction.MonadDatabase m => DFDB.Types.IndexName -> m (Maybe DFDB.Types.Index)
getIndex indexName = use (DFDB.Types.databaseIndices . at indexName)

getIndexOrFail :: DFDB.Transaction.MonadDatabase m => DFDB.Types.IndexName -> m DFDB.Types.Index
getIndexOrFail indexName = maybe (throwError (DFDB.Types.StatementFailureCodeSyntaxError "Index does not exist")) pure
  =<< getIndex indexName

getColumnIndices :: DFDB.Transaction.MonadDatabase m => DFDB.Types.Table -> [DFDB.Types.ColumnName] -> m [Int]
getColumnIndices table cols = do
  let tableName = view DFDB.Types.tableName table
  for cols $ \ col -> case List.elemIndex col (toListOf (DFDB.Types.tableDefinition . each . DFDB.Types.columnDefinitionName) table) of
    Nothing -> throwError $ DFDB.Types.StatementFailureCodeSyntaxError $ "Column " <> DFDB.Types.unColumnName col <> " does not exist in " <> DFDB.Types.unTableName tableName
    Just c -> pure c

getTableIndices :: DFDB.Transaction.MonadDatabase m => DFDB.Types.Table -> m [DFDB.Types.Index]
getTableIndices table =
  traverse
    ( \ indexName ->
      maybe (throwError $ DFDB.Types.StatementFailureCodeInternalError $ "Failed to find index " <> DFDB.Types.unIndexName indexName <> " for table") pure
      =<< use (DFDB.Types.databaseIndices . at indexName) )
    $ view DFDB.Types.tableIndices table

-- |Simple implementation of a query planner. Check the index, and if it matches the where clause exactly use it.
select :: DFDB.Transaction.MonadDatabase m
  => DFDB.Types.Table -> [DFDB.Types.ColumnName] -> [DFDB.Types.WhereClause]
  -> m [[DFDB.Types.Atom]]
select table cols wheres = do
  let whereColumns = toListOf (each . DFDB.Types.whereClauseColumn) wheres
  tableIndexMay <- headMay . filter ((==) whereColumns . view DFDB.Types.indexColumns) <$> getTableIndices table
  maybe (selectTableScan table cols wheres) (\ index -> selectIndex table index cols wheres) tableIndexMay

-- |Select using an index.
selectIndex :: DFDB.Transaction.MonadDatabase m
  => DFDB.Types.Table -> DFDB.Types.Index -> [DFDB.Types.ColumnName] -> [DFDB.Types.WhereClause]
  -> m [[DFDB.Types.Atom]]
selectIndex table index cols wheres = do
  let whereValues = toListOf (each . DFDB.Types.whereClauseValue) wheres
  columnIndices <- getColumnIndices table cols
  pure
    . map (\ (DFDB.Types.Row atoms) -> map ((!!) atoms) columnIndices)
    . fromMaybe mempty
    . DFDB.Tree.lookup whereValues
    . view DFDB.Types.indexData
    $ index

-- |Select using a whole table scan.
selectTableScan :: DFDB.Transaction.MonadDatabase m
  => DFDB.Types.Table -> [DFDB.Types.ColumnName] -> [DFDB.Types.WhereClause]
  -> m [[DFDB.Types.Atom]]
selectTableScan table cols wheres = do
  let whereColumns = toListOf (each . DFDB.Types.whereClauseColumn) wheres
      whereValues = toListOf (each . DFDB.Types.whereClauseValue) wheres
  columnIndices <- getColumnIndices table cols
  whereColumnIndices <- getColumnIndices table whereColumns
  pure
    . map (\ (DFDB.Types.Row atoms) -> map ((!!) atoms) columnIndices)
    . filter (\ (DFDB.Types.Row atoms) -> map ((!!) atoms) whereColumnIndices == whereValues)
    . map snd
    . DFDB.Tree.mapToList
    . view DFDB.Types.tableRows
    $ table

execute :: DFDB.Transaction.MonadDatabase m => DFDB.Types.Statement -> m DFDB.Types.Output
execute = \ case
  -- execute a select
  DFDB.Types.StatementSelect cols tableName wheres -> do
    table <- getTableOrFail tableName
    rows <- select table cols wheres
    pure . DFDB.Types.Output . unlines . map (decodeUtf8 . toStrict . encode) $ rows

  -- execute an insert
  DFDB.Types.StatementInsert row@(DFDB.Types.Row atoms) tableName -> do
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
        let primaryKey = view DFDB.Types.tableNextPrimaryKey table
        assign (DFDB.Types.databaseTables . at tableName . _Just)
          ( over DFDB.Types.tableRows (DFDB.Tree.insertMap primaryKey row)
              . over (DFDB.Types.tableNextPrimaryKey . DFDB.Types._PrimaryKey) (+1)
              $ table
          )
        indices <- use DFDB.Types.databaseIndices
        for_ (toListOf (DFDB.Types.tableIndices . each) table) $ \ indexName ->
          case lookup indexName indices of
            Nothing -> throwError $ DFDB.Types.StatementFailureCodeInternalError $ "Failed to update internal index " <> DFDB.Types.unIndexName indexName <> " for table"
            Just index -> do
              columnIndices <- getColumnIndices table . view DFDB.Types.indexColumns $ index
              assign (DFDB.Types.databaseIndices . at indexName . _Just)
                ( over DFDB.Types.indexData (DFDB.Tree.insertMapWith (<>) (map ((!!) atoms) columnIndices) [row])
                    $ index
                )
        pure $ DFDB.Types.Output "INSERT 1"

  -- execute a create table
  DFDB.Types.StatementCreate tableName cols -> do
    use (DFDB.Types.databaseTables . at tableName) >>= \ case
      Nothing -> do
        modifying DFDB.Types.databaseTables (insertMap tableName $ DFDB.Types.Table tableName cols DFDB.Tree.empty DFDB.Types.initPrimaryKey mempty)
        pure $ DFDB.Types.Output "CREATE TABLE"
      Just _ -> throwError $ DFDB.Types.StatementFailureCodeSyntaxError "Table already exists"

  -- execute a create index
  DFDB.Types.StatementCreateIndex indexName tableName cols -> do
    table <- use (DFDB.Types.databaseTables . at tableName) >>= \ case
      Nothing -> throwError $ DFDB.Types.StatementFailureCodeSyntaxError "Table does not exist"
      Just t -> pure t
    let extraColumns =
          intercalate ", " . map DFDB.Types.unColumnName . setToList . difference (setFromList cols)
            . asSet . setFromList . toListOf (DFDB.Types.tableDefinition . each . DFDB.Types.columnDefinitionName)
            $ table
    unless (null extraColumns) $ throwError $ DFDB.Types.StatementFailureCodeSyntaxError $ "Columns " <> extraColumns <> " not in table"
    modifying (DFDB.Types.databaseTables . at tableName . _Just)
      ( over DFDB.Types.tableIndices (indexName:)
      )
    use (DFDB.Types.databaseIndices . at indexName) >>= \ case
      Nothing -> do
        columnIndices <- getColumnIndices table cols
        let contents = DFDB.Tree.mapFromListWith (<>)
              . map (\ (_, row@(DFDB.Types.Row atoms)) -> (map ((!!) atoms) columnIndices, [row]))
              . DFDB.Tree.mapToList
              . view DFDB.Types.tableRows
              $ table
        modifying DFDB.Types.databaseIndices (insertMap indexName $ DFDB.Types.Index indexName tableName cols contents)
        pure $ DFDB.Types.Output "CREATE INDEX"
      Just _ -> throwError $ DFDB.Types.StatementFailureCodeSyntaxError "Index already exists"

  -- execute a table drop
  DFDB.Types.StatementDrop tableName -> do
    void $ getTableOrFail tableName
    modifying DFDB.Types.databaseTables (deleteMap tableName)
    pure $ DFDB.Types.Output "DROP TABLE"

  -- execute an index drop
  DFDB.Types.StatementDropIndex indexName -> do
    index <- getIndexOrFail indexName
    modifying (DFDB.Types.databaseTables . at (view DFDB.Types.indexTable index) . _Just)
      ( over DFDB.Types.tableIndices (List.delete indexName)
      )
    modifying DFDB.Types.databaseIndices (deleteMap indexName)
    pure $ DFDB.Types.Output "DROP INDEX"
