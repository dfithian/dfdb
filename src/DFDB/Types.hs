module DFDB.Types where

import ClassyPrelude
import Control.Lens.TH (makeLenses)
import Control.Monad (fail)
import Data.Aeson (Value(Bool, Number, String), FromJSON, ToJSON, parseJSON, toJSON)

newtype Command = Command { unCommand :: Text }
  deriving (Eq, Ord, Show)

data CommandFailureCode
  = CommandFailureCodeParser Text
  deriving (Eq, Ord, Show)

data ParsedStatement
  = ParsedStatementHelp
  | ParsedStatement Statement
  deriving (Eq, Ord, Show)

data CommandOutput
  = CommandOutputSuccess ParsedStatement
  | CommandOutputFailure CommandFailureCode
  deriving (Eq, Ord, Show)

data Atom
  = AtomInt Int
  | AtomString Text
  | AtomBool Bool
  deriving (Eq, Ord, Show)

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
    other -> fail $ "Don't know how to parse " <> show other

newtype Row = Row { unRow :: [Atom] }
  deriving (Eq, Ord, Show, ToJSON, FromJSON)

newtype Column = Column { unColumn :: Text }
  deriving (Eq, Ord, Show)

newtype TableName = TableName { unTableName :: Text }
  deriving (Eq, Ord, Show)

pageSize :: Int
pageSize = 4096

data Table = Table
  { _tableName    :: TableName
  , _tableColumns :: [Column]
  , _tableRows    :: [Row]
  }
  deriving (Eq, Ord, Show)

data Database = Database
  { _databaseTables :: Map TableName Table
  }
  deriving (Eq, Ord, Show)

data Statement
  = StatementSelect [Column] TableName
  | StatementInsert Row TableName
  | StatementCreate TableName [Column]
  deriving (Eq, Ord, Show)

data StatementFailureCode
  = StatementFailureCodeSyntaxError Text
  deriving (Eq, Ord, Show)

newtype Output = Output { unOutput :: Text }
  deriving (Eq, Ord, Show)

data StatementResult
  = StatementResultSuccess Output
  | StatementResultFailure StatementFailureCode
  deriving (Eq, Ord, Show)

makeLenses ''Table
makeLenses ''Database
