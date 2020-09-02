module DFDB.Types where

import ClassyPrelude

newtype Command = Command { unCommand :: Text }
  deriving (Eq, Ord, Show)

data FailureCode
  = FailureCodeParser Text
  deriving (Eq, Ord, Show)

data CommandOutput
  = CommandOutputSuccess Statement
  | CommandOutputFailure FailureCode
  deriving (Eq, Ord, Show)

data Statement
  = StatementHelp
  | StatementSelect ByteString
  | StatementInsert ByteString
  deriving (Eq, Ord, Show)
