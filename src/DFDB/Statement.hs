module DFDB.Statement where

import ClassyPrelude
import qualified Data.Attoparsec.ByteString as Atto

import qualified DFDB.Types

runParser :: Atto.Parser DFDB.Types.Statement -> DFDB.Types.Command -> DFDB.Types.CommandOutput
runParser parser (DFDB.Types.Command command) = case Atto.parse parser $ encodeUtf8 command of
  Atto.Done _ statement -> DFDB.Types.CommandOutputSuccess statement
  Atto.Fail _ _ err -> DFDB.Types.CommandOutputFailure $ DFDB.Types.FailureCodeParser $ pack err
  Atto.Partial f -> case f "" of
    Atto.Done _ statement -> DFDB.Types.CommandOutputSuccess statement
    Atto.Fail _ _ err -> DFDB.Types.CommandOutputFailure $ DFDB.Types.FailureCodeParser $ pack err
    Atto.Partial _ -> DFDB.Types.CommandOutputFailure $ DFDB.Types.FailureCodeParser "returned partial result"

spaces :: Atto.Parser ()
spaces = void $ Atto.many' (Atto.string " ")

tillSemicolon :: Atto.Parser ByteString
tillSemicolon = Atto.takeTill ((==) 59)

keyword :: ByteString -> Atto.Parser ByteString
keyword k = spaces *> (Atto.string k <* spaces)

parseStatement :: Atto.Parser DFDB.Types.Statement
parseStatement =
  (DFDB.Types.StatementHelp <$ keyword "help")
    <|> (DFDB.Types.StatementSelect <$ keyword "select" <*> Atto.takeByteString)
    <|> (DFDB.Types.StatementInsert <$ keyword "insert" <*> Atto.takeByteString)
