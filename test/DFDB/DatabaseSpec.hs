module DFDB.DatabaseSpec where

import ClassyPrelude
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Control.Monad.State (State, runState)
import Test.Hspec (Expectation, HasCallStack, Spec, describe, it, shouldBe, expectationFailure)

import qualified DFDB.Database as Database
import qualified DFDB.Tree
import qualified DFDB.Types

isSuccess :: DFDB.Types.StatementResult -> Expectation
isSuccess = \ case
  DFDB.Types.StatementResultSuccess _ -> pure ()
  DFDB.Types.StatementResultFailure err -> expectationFailure $ show err

isSuccessWith :: DFDB.Types.Output -> DFDB.Types.StatementResult -> Expectation
isSuccessWith out = \ case
  DFDB.Types.StatementResultSuccess actualOut -> actualOut `shouldBe` out
  DFDB.Types.StatementResultFailure err -> expectationFailure $ show err

checkDatabase
  :: (HasCallStack)
  => (DFDB.Types.Database -> Expectation)
  -> (DFDB.Types.StatementResult -> Expectation)
  -> State DFDB.Types.Database DFDB.Types.StatementResult
  -> Expectation
checkDatabase checkDb checkResult mx = do
  let (result, db) = runState mx Database.emptyDatabase
  checkDb db
  checkResult result

checkSuccess
  :: (HasCallStack)
  => DFDB.Types.Database
  -> State DFDB.Types.Database DFDB.Types.StatementResult
  -> Expectation
checkSuccess db = checkDatabase (flip shouldBe db) isSuccess

checkSuccessWith
  :: (HasCallStack)
  => DFDB.Types.Database
  -> DFDB.Types.Output
  -> State DFDB.Types.Database DFDB.Types.StatementResult
  -> Expectation
checkSuccessWith db out = checkDatabase (flip shouldBe db) (isSuccessWith out)

sequenceStatements :: NonEmpty (State DFDB.Types.Database DFDB.Types.StatementResult) -> State DFDB.Types.Database DFDB.Types.StatementResult
sequenceStatements = \ case
  mx :| [] -> mx
  mx :| (mx2 : mxs) -> mx >>= \ case
    DFDB.Types.StatementResultSuccess _ -> sequenceStatements $ mx2 :| mxs
    result@(DFDB.Types.StatementResultFailure _) -> pure result

spec :: Spec
spec = describe "Database" $ do

  let fooTableName = DFDB.Types.TableName "foo"
      barColumnName = DFDB.Types.ColumnName "bar"
      barColumnDef = DFDB.Types.ColumnDefinition barColumnName DFDB.Types.AtomTypeInt
      binColumnName = DFDB.Types.ColumnName "bin"
      binColumnDef = DFDB.Types.ColumnDefinition binColumnName DFDB.Types.AtomTypeString
      row = DFDB.Types.Row [DFDB.Types.AtomInt 1, DFDB.Types.AtomString "qux"]
      bazIndexName = DFDB.Types.IndexName "baz"

  it "creates a table" $
    let db = DFDB.Types.Database (singletonMap fooTableName $ DFDB.Types.Table fooTableName [barColumnDef, binColumnDef] DFDB.Tree.empty DFDB.Types.initPrimaryKey mempty) mempty
    in checkSuccess db $
      Database.execute $
        DFDB.Types.StatementCreate fooTableName [barColumnDef, binColumnDef]

  it "inserts to a table" $
    let db = DFDB.Types.Database (singletonMap fooTableName $ DFDB.Types.Table fooTableName [barColumnDef, binColumnDef] (DFDB.Tree.singletonMap DFDB.Types.initPrimaryKey row) (DFDB.Types.PrimaryKey 2) mempty) mempty
    in checkSuccess db $
      sequenceStatements . NonEmpty.fromList $
        [ Database.execute $ DFDB.Types.StatementCreate fooTableName [barColumnDef, binColumnDef]
        , Database.execute $ DFDB.Types.StatementInsert row fooTableName
        ]

  it "selects from a table" $
    let db = DFDB.Types.Database (singletonMap fooTableName $ DFDB.Types.Table fooTableName [barColumnDef, binColumnDef] (DFDB.Tree.singletonMap DFDB.Types.initPrimaryKey row) (DFDB.Types.PrimaryKey 2) mempty) mempty
        out = DFDB.Types.Output "[\"qux\"]\n"
    in checkSuccessWith db out $
      sequenceStatements . NonEmpty.fromList $
        [ Database.execute $ DFDB.Types.StatementCreate fooTableName [barColumnDef, binColumnDef]
        , Database.execute $ DFDB.Types.StatementInsert row fooTableName
        , Database.execute $ DFDB.Types.StatementSelect [binColumnName] fooTableName
        ]

  it "drops a table" $
    checkSuccess Database.emptyDatabase $
      sequenceStatements . NonEmpty.fromList $
        [ Database.execute $ DFDB.Types.StatementCreate fooTableName [barColumnDef]
        , Database.execute $ DFDB.Types.StatementDrop fooTableName
        ]

  it "creates an empty index" $
    let bazIndex = DFDB.Types.Index bazIndexName fooTableName [barColumnName] DFDB.Tree.empty
        db = DFDB.Types.Database (singletonMap fooTableName $ DFDB.Types.Table fooTableName [barColumnDef, binColumnDef] DFDB.Tree.empty DFDB.Types.initPrimaryKey [bazIndexName]) (singletonMap bazIndexName bazIndex)
    in checkSuccess db $
      sequenceStatements . NonEmpty.fromList $
        [ Database.execute $ DFDB.Types.StatementCreate fooTableName [barColumnDef, binColumnDef]
        , Database.execute $ DFDB.Types.StatementCreateIndex bazIndexName fooTableName [barColumnName]
        ]

  it "creates an index after inserting" $
    let bazIndex = DFDB.Types.Index bazIndexName fooTableName [barColumnName] (DFDB.Tree.singletonMap [DFDB.Types.AtomInt 1] DFDB.Types.initPrimaryKey)
        db = DFDB.Types.Database (singletonMap fooTableName $ DFDB.Types.Table fooTableName [barColumnDef, binColumnDef] (DFDB.Tree.singletonMap DFDB.Types.initPrimaryKey row) (DFDB.Types.PrimaryKey 2) [bazIndexName]) (singletonMap bazIndexName bazIndex)
    in checkSuccess db $
      sequenceStatements . NonEmpty.fromList $
        [ Database.execute $ DFDB.Types.StatementCreate fooTableName [barColumnDef, binColumnDef]
        , Database.execute $ DFDB.Types.StatementInsert row fooTableName
        , Database.execute $ DFDB.Types.StatementCreateIndex bazIndexName fooTableName [barColumnName]
        ]

  it "creates an index before inserting" $
    let bazIndex = DFDB.Types.Index bazIndexName fooTableName [barColumnName] (DFDB.Tree.singletonMap [DFDB.Types.AtomInt 1] DFDB.Types.initPrimaryKey)
        db = DFDB.Types.Database (singletonMap fooTableName $ DFDB.Types.Table fooTableName [barColumnDef, binColumnDef] (DFDB.Tree.singletonMap DFDB.Types.initPrimaryKey row) (DFDB.Types.PrimaryKey 2) [bazIndexName]) (singletonMap bazIndexName bazIndex)
    in checkSuccess db $
      sequenceStatements . NonEmpty.fromList $
        [ Database.execute $ DFDB.Types.StatementCreate fooTableName [barColumnDef, binColumnDef]
        , Database.execute $ DFDB.Types.StatementCreateIndex bazIndexName fooTableName [barColumnName]
        , Database.execute $ DFDB.Types.StatementInsert row fooTableName
        ]

  it "drops an index" $
    let db = DFDB.Types.Database (singletonMap fooTableName $ DFDB.Types.Table fooTableName [barColumnDef, binColumnDef] DFDB.Tree.empty DFDB.Types.initPrimaryKey mempty) mempty
    in checkSuccess db $
      sequenceStatements . NonEmpty.fromList $
        [ Database.execute $ DFDB.Types.StatementCreate fooTableName [barColumnDef, binColumnDef]
        , Database.execute $ DFDB.Types.StatementCreateIndex bazIndexName fooTableName [barColumnName]
        , Database.execute $ DFDB.Types.StatementDropIndex bazIndexName
        ]
