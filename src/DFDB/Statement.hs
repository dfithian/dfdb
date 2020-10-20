module DFDB.Statement where

import ClassyPrelude hiding (index)
import Prelude (read)
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString.Char8 as C8

import qualified DFDB.Types

runParser :: Atto.Parser DFDB.Types.ParsedStatement -> DFDB.Types.Command -> DFDB.Types.CommandOutput
runParser parser (DFDB.Types.Command command) = case Atto.parse parser $ encodeUtf8 command of
  Atto.Done _ statement -> DFDB.Types.CommandOutputSuccess statement
  Atto.Fail _ _ err -> DFDB.Types.CommandOutputFailure $ DFDB.Types.CommandFailureCodeParser $ pack err
  Atto.Partial f -> case f "" of
    Atto.Done _ statement -> DFDB.Types.CommandOutputSuccess statement
    Atto.Fail _ _ err -> DFDB.Types.CommandOutputFailure $ DFDB.Types.CommandFailureCodeParser $ pack err
    Atto.Partial _ -> DFDB.Types.CommandOutputFailure $ DFDB.Types.CommandFailureCodeParser "returned partial result"

isAlpha :: Word8 -> Bool
isAlpha w = (w >= 65 && w <= 90) || (w >= 97 && w <= 122)

isDigit :: Word8 -> Bool
isDigit w = w >= 48 && w <= 57

isAlphaOrUnderscore :: Word8 -> Bool
isAlphaOrUnderscore w = isAlpha w || w == 95

spaces :: Atto.Parser ()
spaces = void $ Atto.many' (Atto.string " ")

tillSemicolon :: Atto.Parser ByteString
tillSemicolon = Atto.takeTill ((==) 59)

keyword :: ByteString -> Atto.Parser ByteString
keyword k = spaces *> (Atto.string k <* spaces)

alphaOrUnderscore :: Atto.Parser ByteString
alphaOrUnderscore = spaces *> Atto.takeWhile1 isAlphaOrUnderscore

digit :: Atto.Parser Int
digit = read . C8.unpack <$> (spaces *> Atto.takeWhile1 isDigit) <* spaces

literal :: Atto.Parser ByteString
literal = (spaces *> Atto.string "\'" *> Atto.takeWhile1 (not . (==) 39)) <* Atto.string "\'" <* spaces

boolean :: Atto.Parser Bool
boolean = (spaces *> inner) <* spaces
  where inner = (True <$ Atto.string "true") <|> (False <$ Atto.string "false")

atom :: Atto.Parser DFDB.Types.Atom
atom =
  (DFDB.Types.AtomInt <$> digit)
    <|> (DFDB.Types.AtomString . decodeUtf8 <$> literal)
    <|> (DFDB.Types.AtomBool <$> boolean)

list :: Atto.Parser a -> Atto.Parser [a]
list p = (spaces *> Atto.string "(" *> Atto.many' (p <* Atto.string ",")) <* Atto.string ")" <* spaces

row :: Atto.Parser DFDB.Types.Row
row = DFDB.Types.Row <$> list atom

columnName :: Atto.Parser DFDB.Types.ColumnName
columnName = DFDB.Types.ColumnName . decodeUtf8 <$> alphaOrUnderscore

columnNames :: Atto.Parser [DFDB.Types.ColumnName]
columnNames = list columnName

whereClause :: Atto.Parser DFDB.Types.WhereClause
whereClause = DFDB.Types.WhereClause <$> columnName <*> (keyword "=" *> atom)

wheres :: Atto.Parser [DFDB.Types.WhereClause]
wheres = list whereClause

atomType :: Atto.Parser DFDB.Types.AtomType
atomType =
  (DFDB.Types.AtomTypeInt <$ Atto.string "int")
    <|> (DFDB.Types.AtomTypeString <$ Atto.string "string")
    <|> (DFDB.Types.AtomTypeBool <$ Atto.string "bool")

columnDefinition :: Atto.Parser DFDB.Types.ColumnDefinition
columnDefinition = DFDB.Types.ColumnDefinition <$> columnName <*> ((spaces *> atomType) <* spaces)

columnDefinitions :: Atto.Parser [DFDB.Types.ColumnDefinition]
columnDefinitions = list columnDefinition

table :: Atto.Parser DFDB.Types.TableName
table = DFDB.Types.TableName . decodeUtf8 <$> alphaOrUnderscore

index :: Atto.Parser DFDB.Types.IndexName
index = DFDB.Types.IndexName . decodeUtf8 <$> alphaOrUnderscore

parseStatement :: Atto.Parser DFDB.Types.ParsedStatement
parseStatement =
  (DFDB.Types.ParsedStatementHelp <$ keyword "help")
    <|> ( DFDB.Types.ParsedStatement
            <$> ( (DFDB.Types.StatementSelect <$ keyword "select" <*> columnNames <*> table <*> (keyword "where" *> wheres) <* spaces <* Atto.string ";" <* Atto.takeByteString)
                     <|> (DFDB.Types.StatementInsert <$ keyword "insert" <*> row <*> table <* spaces <* Atto.string ";" <* Atto.takeByteString)
                     <|> (DFDB.Types.StatementCreate <$ keyword "create table" <*> table <*> columnDefinitions <* spaces <* Atto.string ";" <* Atto.takeByteString)
                     <|> (DFDB.Types.StatementCreateIndex <$ keyword "create index" <*> index <*> table <*> columnNames <* spaces <* Atto.string ";" <* Atto.takeByteString)
                     <|> (DFDB.Types.StatementDrop <$ keyword "drop table" <*> table <* spaces <* Atto.string ";" <* Atto.takeByteString)
                     <|> (DFDB.Types.StatementDropIndex <$ keyword "drop index" <*> index <* spaces <* Atto.string ";" <* Atto.takeByteString)
                )
        )
