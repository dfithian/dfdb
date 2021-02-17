{-# OPTIONS_GHC -fno-warn-orphans #-}
import ClassyPrelude hiding (index)
import Control.Monad.Except (runExcept)
import Control.Monad.State (runStateT)
import Criterion.Main (bench, bgroup, defaultMain, env, nf)
import Test.QuickCheck (Gen, arbitrary, choose, elements, generate, listOf1, vectorOf)

import DFDB.Database (selectIndex, selectPK, selectTableScan)
import qualified DFDB.Tree
import qualified DFDB.Types

instance NFData DFDB.Tree.Color
instance (NFData a, NFData b) => NFData (DFDB.Tree.TreeMap a b)

instance NFData DFDB.Types.Atom
instance NFData DFDB.Types.AtomType
instance NFData DFDB.Types.ColumnName
instance NFData DFDB.Types.ColumnDefinition
instance NFData DFDB.Types.PrimaryKey
instance NFData DFDB.Types.Row
instance NFData DFDB.Types.TableName
instance NFData DFDB.Types.Table
instance NFData DFDB.Types.IndexName
instance NFData DFDB.Types.Index

peopleTableName :: DFDB.Types.TableName
peopleTableName = DFDB.Types.TableName "people"

nameColumnName, ageColumnName, likesDogsColumnName :: DFDB.Types.ColumnName
nameColumnName = DFDB.Types.ColumnName "name"
ageColumnName = DFDB.Types.ColumnName "age"
likesDogsColumnName = DFDB.Types.ColumnName "likes_dogs"

nameColumn, ageColumn, likesDogsColumn :: DFDB.Types.ColumnDefinition
nameColumn = DFDB.Types.ColumnDefinition nameColumnName DFDB.Types.AtomTypeString
ageColumn = DFDB.Types.ColumnDefinition ageColumnName DFDB.Types.AtomTypeInt
likesDogsColumn = DFDB.Types.ColumnDefinition likesDogsColumnName DFDB.Types.AtomTypeBool

ageIndexName :: DFDB.Types.IndexName
ageIndexName = DFDB.Types.IndexName "age_idx"

peopleTable :: DFDB.Tree.TreeMap DFDB.Types.PrimaryKey DFDB.Types.Row -> DFDB.Types.Table
peopleTable rows = DFDB.Types.Table peopleTableName [nameColumn, ageColumn, likesDogsColumn] rows DFDB.Types.initPrimaryKey [ageIndexName]

ageIndex :: DFDB.Tree.TreeMap [DFDB.Types.Atom] [DFDB.Types.Row] -> DFDB.Types.Index
ageIndex contents = DFDB.Types.Index ageIndexName peopleTableName [ageColumnName] contents

arbitraryAlphaNumericChar :: Gen Char
arbitraryAlphaNumericChar = elements . mconcat $
  [ ['A'..'Z']
  , ['a'..'z']
  , ['0'..'9']
  ]

nonEmptyArbitraryAlphaNumericText :: Gen Text
nonEmptyArbitraryAlphaNumericText = pack <$> listOf1 arbitraryAlphaNumericChar

arbitraryString, arbitraryInt, arbitraryBool :: Gen DFDB.Types.Atom
arbitraryString = DFDB.Types.AtomString <$> nonEmptyArbitraryAlphaNumericText
arbitraryInt = DFDB.Types.AtomInt <$> choose (0, 100)
arbitraryBool = DFDB.Types.AtomBool <$> arbitrary

-- |Generate `n` rows of the `people` table, index by age, and return a row that's in the data set to use to benchmark a
-- query.
genData :: Int -> IO (DFDB.Types.Table, DFDB.Types.Index, DFDB.Types.PrimaryKey, (DFDB.Types.Atom, DFDB.Types.Atom, DFDB.Types.Atom))
genData n = generate $ do
  let genTraits = (,,) <$> arbitraryString <*> arbitraryInt <*> arbitraryBool
      toPerson (name, age, likesDogs) = DFDB.Types.Row [name, age, likesDogs]
  oneTrait <- genTraits
  pkey <- DFDB.Types.PrimaryKey <$> choose (1, n - 1)
  allTraits <- (oneTrait:) <$> vectorOf n genTraits
  let peopleByPKey = DFDB.Tree.mapFromList $ zip (DFDB.Types.PrimaryKey <$> [1..]) (map toPerson allTraits)
      peopleByAge = DFDB.Tree.mapFromListWith (<>) $ flip map allTraits $ \ traits@(_, age, _) -> ([age], [toPerson traits])
  pure (peopleTable peopleByPKey, ageIndex peopleByAge, pkey, oneTrait)

-- |Run a test. If the index is provided, query using the index. Filter using the provided atom in the `where` clause.
runTest :: DFDB.Types.Table -> Maybe DFDB.Types.Index -> DFDB.Types.Atom -> ()
runTest table indexMay age =
  let db = DFDB.Types.Database (singletonMap peopleTableName table) (maybe mempty (singletonMap ageIndexName) indexMay)
      cols = [nameColumnName, ageColumnName, likesDogsColumnName]
      wheres = [DFDB.Types.WhereClause ageColumnName age]
      result = runExcept
        . flip runStateT (db, Nothing)
        . maybe (selectTableScan table cols wheres) (\ index -> selectIndex table index cols wheres)
        $ indexMay
  in case result of
    Left err -> error $ "Got error " <> show err <> " with database " <> show db
    Right _ -> ()

-- |Run a primary key test.
runTestPK :: DFDB.Types.Table -> DFDB.Types.PrimaryKey -> ()
runTestPK table pkey =
  let db = DFDB.Types.Database (singletonMap peopleTableName table) mempty
      cols = [nameColumnName, ageColumnName, likesDogsColumnName]
      result = runExcept
        . flip runStateT (db, Nothing)
        . selectPK table cols
        $ pkey
  in case result of
    Left err -> error $ "Got error " <> show err <> " with database " <> show db
    Right _ -> ()

main :: IO ()
main = defaultMain
  [ env (genData 10000) $ \ ~(table, index, pkey, (_, age, _)) -> bgroup "select - 10000"
      [ bench "by pk" $ nf (runTestPK table) pkey
      , bench "no index" $ nf (runTest table Nothing) age
      , bench "using index" $ nf (runTest table $ Just index) age
      ]
  , env (genData 100000) $ \ ~(table, index, pkey, (_, age, _)) -> bgroup "select - 100000"
      [ bench "by pk" $ nf (runTestPK table) pkey
      , bench "no index" $ nf (runTest table Nothing) age
      , bench "using index" $ nf (runTest table $ Just index) age
      ]
  , env (genData 200000) $ \ ~(table, index, pkey, (_, age, _)) -> bgroup "select - 200000"
      [ bench "by pk" $ nf (runTestPK table) pkey
      , bench "no index" $ nf (runTest table Nothing) age
      , bench "using index" $ nf (runTest table $ Just index) age
      ]
  ]
