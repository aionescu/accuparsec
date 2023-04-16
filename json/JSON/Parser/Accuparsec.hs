module JSON.Parser.Accuparsec(parse) where

import Control.Applicative((<|>), many)
import Control.Applicative.Combinators(between, skipMany, sepBy)
import Data.Accuparsec.Text(Parser, ErrorList, (<?>), endOfInput, decimal, satisfy, signed, skipSpace, takeWhile, char, endOfLine, runParser, string)
import Data.Function(on)
import Data.Map.Strict qualified as M
import Data.Text(Text)
import Data.Text qualified as T
import Prelude hiding (takeWhile)

import JSON.Syntax

ws :: Parser ()
ws = skipSpace *> skipMany (comment *> skipSpace) <?> "whitespace or comment"
  where
    comment = string "//" *> takeWhile (\c -> c /= '\r' && c /= '\n') *> endOfLine

lexeme :: Parser a -> Parser a
lexeme p = p <* ws

symbol :: Text -> Parser Text
symbol s = string s <* ws

btwn :: Text -> Text -> Parser a -> Parser a
btwn = between `on` symbol

quotedString :: Parser Text
quotedString = T.pack <$> lexeme (between (char '"') (char '"') (many $ escaped <|> regular)) <?> "quoted string"
  where
    escaped = char '\\' *> (unescape <$> satisfy \c -> c == '\\' || c == '"' || c == '\n' || c == '\r' || c == '\t')
    regular = satisfy \c -> c /= '\\' && c /= '"' && c /= '\n' && c /= '\r' && c /= '\t'

    unescape = \case
      '\\' -> '\\'
      '"' -> '"'
      'n' -> '\n'
      'r' -> '\r'
      't' -> '\t'
      a -> a

object :: Parser [(Text, JSON)]
object = btwn "{" "}" $ field `sepBy` symbol ","
  where
    field = (,) <$> quotedString <* symbol ":" <*> json

checkDuplicates :: [(Text, JSON)] -> Parser JSON
checkDuplicates fields
  | M.size map == length fields = pure $ Object map
  | otherwise = fail "duplicate field"
  where
    map = M.fromList fields

json :: Parser JSON
json =
  Null <$ symbol "null"
  <|> Bool True <$ symbol "true"
  <|> Bool False <$ symbol "false"
  <|> Number <$> lexeme (signed decimal)
  <|> String <$> quotedString
  <|> Array <$> btwn "[" "]" (json `sepBy` symbol ",")
  <|> (object >>= checkDuplicates)

parse :: Text -> Either ErrorList JSON
parse = runParser $ ws *> json <* endOfInput
