module DFDB.Types where

import ClassyPrelude hiding (Index)
import Control.Lens.TH (makeLenses, makePrisms)
import Control.Monad (fail)
import Data.Aeson
  ( Value(Bool, Number, String), (.:), (.=), FromJSON, FromJSONKey, ToJSON, ToJSONKey, object
  , parseJSON, toJSON, withObject
  )

import DFDB.Tree (TreeMap)

-- |User input.
newtype Command = Command { unCommand :: Text }
  deriving (Eq, Ord, Show)

-- |Failure to parse a command.
data CommandFailureCode
  = CommandFailureCodeParser Text
  deriving (Eq, Ord, Show)

-- |Success parsing a command.
data ParsedStatement
  = ParsedStatementHelp
  | ParsedStatement Statement
  deriving (Eq, Ord, Show)

-- |Result of parsing a command.
data CommandOutput
  = CommandOutputSuccess ParsedStatement
  | CommandOutputFailure CommandFailureCode
  deriving (Eq, Ord, Show)

-- |Meta type for an atom (primitive).
data AtomType
  = AtomTypeInt
  | AtomTypeString
  | AtomTypeBool
  deriving (Eq, Ord)

instance Show AtomType where
  show = \ case
    AtomTypeInt -> "int"
    AtomTypeString -> "string"
    AtomTypeBool -> "bool"

-- |Atom (primitive) value.
data Atom
  = AtomInt Int
  | AtomString Text
  | AtomBool Bool
  deriving (Eq, Ord)

instance Show Atom where
  show = \ case
    AtomInt i -> show i
    AtomString s -> show s
    AtomBool b -> show b

-- |A primary key.
newtype PrimaryKey = PrimaryKey { unPrimaryKey :: Int }
  deriving (Eq, Ord, Show, ToJSON, FromJSON)

-- |A row of values in a table.
newtype Row = Row { unRow :: [Atom] }
  deriving (Eq, Ord, Show)

-- |The name of a column in a table, used for querying.
newtype ColumnName = ColumnName { unColumnName :: Text }
  deriving (Eq, Ord, Show, ToJSON, FromJSON)

-- |Column definition in a table.
data ColumnDefinition = ColumnDefinition
  { _columnDefinitionName :: ColumnName
  -- ^ Name of the column.
  , _columnDefinitionType :: AtomType
  -- ^ Type of the column.
  }
  deriving (Eq, Ord, Show)

-- |Name of a table, used for creating/querying/inserting.
newtype TableName = TableName { unTableName :: Text }
  deriving (Eq, Ord, Show, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

-- |A table, including its name, definition, and data.
data Table = Table
  { _tableName           :: TableName
  -- ^ The name of the table.
  , _tableDefinition     :: [ColumnDefinition]
  -- ^ The column definitions of the table.
  , _tableRows           :: TreeMap PrimaryKey Row
  -- ^ The data in the table.
  , _tableNextPrimaryKey :: PrimaryKey
  -- ^ The next primary key in the table.
  , _tableIndices        :: [IndexName]
  -- ^ Indices for the table.
  }
  deriving (Eq, Ord, Show)

-- |Name of an index.
newtype IndexName = IndexName { unIndexName :: Text }
  deriving (Eq, Ord, Show, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

-- |An index, including its name, table, and definition.
data Index = Index
  { _indexName    :: IndexName
  -- ^ The name of the index.
  , _indexTable   :: TableName
  -- ^ The name of the table.
  , _indexColumns :: [ColumnName]
  -- ^ The name of the columns in the index.
  , _indexData    :: TreeMap [Atom] PrimaryKey
  -- ^ The data in the table.
  }
  deriving (Eq, Ord, Show)

-- |Our in-memory database.
data Database = Database
  { _databaseTables  :: Map TableName Table
  , _databaseIndices :: Map IndexName Index
  }
  deriving (Eq, Ord, Show)

-- |A statement to execute against a 'Database'.
data Statement
  = StatementSelect [ColumnName] TableName
  | StatementInsert Row TableName
  | StatementCreate TableName [ColumnDefinition]
  | StatementCreateIndex IndexName TableName [ColumnName]
  | StatementDrop TableName
  | StatementDropIndex IndexName
  deriving (Eq, Ord, Show)

-- |A statement failed to execute.
data StatementFailureCode
  = StatementFailureCodeSyntaxError Text
  | StatementFailureCodeInternalError Text
  deriving (Eq, Ord, Show)

-- |An executed statement's output.
newtype Output = Output { unOutput :: Text }
  deriving (Eq, Ord, Show)

-- |The result of running a statement.
data StatementResult
  = StatementResultSuccess Output
  | StatementResultFailure StatementFailureCode
  deriving (Eq, Ord, Show)

makePrisms ''PrimaryKey
makeLenses ''ColumnDefinition
makeLenses ''Table
makeLenses ''Index
makeLenses ''Database

instance ToJSON AtomType where
  toJSON = \ case
    AtomTypeInt -> String "int"
    AtomTypeString -> String "string"
    AtomTypeBool -> String "bool"

instance FromJSON AtomType where
  parseJSON = \ case
    String "int" -> pure AtomTypeInt
    String "string" -> pure AtomTypeString
    String "bool" -> pure AtomTypeBool
    other -> fail $ "Unknown atom type " <> show other

instance ToJSON Atom where
  toJSON = \ case
    AtomInt i -> Number $ fromIntegral i
    AtomString t -> String t
    AtomBool b -> Bool b

instance FromJSON Atom where
  parseJSON = \ case
    Number s -> pure $ AtomInt $ round s
    String t -> pure $ AtomString t
    Bool b -> pure $ AtomBool b
    other -> fail $ "Unknown atom " <> show other

deriving instance ToJSON Row
deriving instance FromJSON Row

instance ToJSON ColumnDefinition where
  toJSON (ColumnDefinition name typs) = object
    [ "name" .= name
    , "types" .= typs
    ]

instance FromJSON ColumnDefinition where
  parseJSON = withObject "ColumnDefinition" $ \ obj ->
    ColumnDefinition <$> obj .: "name" <*> obj .: "types"

instance ToJSON Table where
  toJSON (Table name definition rows nextPrimaryKey indices) = object
    [ "name" .= name
    , "definition" .= definition
    , "rows" .= rows
    , "nextPrimaryKey" .= nextPrimaryKey
    , "indices" .= indices
    ]

instance FromJSON Table where
  parseJSON = withObject "Table" $ \ obj ->
    Table <$> obj .: "name" <*> obj .: "definition" <*> obj .: "rows" <*> obj .: "nextPrimaryKey" <*> obj .: "indices"

instance ToJSON Index where
  toJSON (Index name table columns contents) = object
    [ "name" .= name
    , "table" .= table
    , "columns" .= columns
    , "data" .= contents
    ]

instance FromJSON Index where
  parseJSON = withObject "Index" $ \ obj ->
    Index <$> obj .: "name" <*> obj .: "table" <*> obj .: "columns" <*> obj .: "data"

instance ToJSON Database where
  toJSON (Database tables indices) = object
    [ "tables" .= tables
    , "indices" .= indices
    ]

instance FromJSON Database where
  parseJSON = withObject "Database" $ \ obj ->
    Database <$> obj .: "tables" <*> obj .: "indices"

toAtomType :: Atom -> AtomType
toAtomType = \ case
  AtomInt _ -> AtomTypeInt
  AtomString _ -> AtomTypeString
  AtomBool _ -> AtomTypeBool

initPrimaryKey :: PrimaryKey
initPrimaryKey = PrimaryKey 1
