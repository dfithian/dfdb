module DFDB.Statement where

import ClassyPrelude
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

column :: Atto.Parser DFDB.Types.Column
column = DFDB.Types.Column . decodeUtf8 <$> alphaOrUnderscore

columns :: Atto.Parser [DFDB.Types.Column]
columns = list column

table :: Atto.Parser DFDB.Types.TableName
table = DFDB.Types.TableName . decodeUtf8 <$> alphaOrUnderscore

parseStatement :: Atto.Parser DFDB.Types.ParsedStatement
parseStatement =
  (DFDB.Types.ParsedStatementHelp <$ keyword "help")
    <|> ( DFDB.Types.ParsedStatement
            <$> ( (DFDB.Types.StatementSelect <$ keyword "select" <*> columns <*> table <* spaces <* Atto.string ";" <* Atto.takeByteString)
                     <|> (DFDB.Types.StatementInsert <$ keyword "insert" <*> row <*> table <* spaces <* Atto.string ";" <* Atto.takeByteString)
                     <|> (DFDB.Types.StatementCreate <$ keyword "create table" <*> table <*> columns <* spaces <* Atto.string ";" <* Atto.takeByteString)
                )
        )
