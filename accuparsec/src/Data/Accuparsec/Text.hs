module Data.Accuparsec.Text where

import Control.Applicative(Alternative(..))
import Control.Monad(MonadPlus)
import Control.Monad.State.Class(MonadState(..))
import Control.Monad.Trans.Maybe(MaybeT(..))
import Control.Monad.Trans.State.Strict(StateT(..))
import Control.Monad.Trans.Writer.Strict(WriterT(..))
import Control.Monad.Writer.Class(MonadWriter(..))
import Data.Char(isDigit, isLetter, ord, isSpace)
import Data.Functor(void)
import Data.Functor.Identity(Identity(..))
import Data.Text(Text)
import Data.Text qualified as T

import Data.DList

showT :: Show a => a -> Text
showT = T.pack . show

data ParseError
  = Error { expected :: Text, remainingInput :: Text }
  | Label { label :: Text, remainingInput :: Text }

type ErrorList = DList ParseError

newtype Parser a =
  Parser
  { unParser :: Text -> (Maybe (a, Text), ErrorList)
  }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadState Text, MonadWriter ErrorList)
  via StateT Text (MaybeT (WriterT ErrorList Identity))

signalError :: ParseError -> Parser ()
signalError err = Parser $ const (Nothing, singleton err)

instance MonadFail Parser where
  fail :: String -> Parser a
  fail msg = Parser \input -> (Nothing, singleton $ Label (T.pack msg) input)

runParser :: Text -> Parser a -> Either [ParseError] a
runParser input (Parser p) =
  case p input of
    (Nothing, errs) -> Left $ toList errs
    (Just (a, _), _) -> Right a

(<?>) :: Parser a -> Text -> Parser a
p <?> lbl = Parser \input ->
  case unParser p input of
    (Nothing, _) -> (Nothing, singleton $ Label lbl input)
    (Just (a, rest), errs) -> (Just (a, rest), errs) -- TODO: add label to errs?
infix 0 <?>

endOfInput :: Parser ()
endOfInput = Parser \input ->
  if T.null input
  then (Just ((), input), mempty)
  else (Nothing, singleton $ Error "end of input" input)

char :: Char -> Parser Char
char c = Parser \input ->
  case T.uncons input of
    r@(Just (c', _)) | c == c' -> (r, mempty)
    _ -> (Nothing, singleton $ Error (showT c) input)

string :: Text -> Parser Text
string s = Parser \input ->
  if s `T.isPrefixOf` input
  then (Just (s, T.drop (T.length s) input), mempty)
  else (Nothing, singleton $ Error (showT s) input)

anyChar :: Parser Char
anyChar = Parser \input ->
  case T.uncons input of
    r@Just{} -> (r, mempty)
    Nothing -> (Nothing, singleton $ Error "any character" input)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser \input ->
  case T.uncons input of
    r@(Just (c, _)) | p c -> (r, mempty)
    _ -> (Nothing, singleton $ Error "satisfy" input)

digit :: Parser Char
digit = satisfy isDigit

letter :: Parser Char
letter = satisfy isLetter

takeWhile1 :: (Char -> Bool) -> Parser Text
takeWhile1 p = Parser \input ->
  let r@(prefix, _) = T.span p input
  in
    if T.null prefix
    then (Nothing, singleton $ Label "takeWhile1" input)
    else (Just r, mempty)

decimal :: Integral a => Parser a
decimal = T.foldl' step 0 <$> takeWhile1 isDigit <?> "decimal"
  where
    step a c = a * 10 + fromIntegral (ord c - 48)

signed :: Num a => Parser a -> Parser a
signed p = sign <*> p
  where
    sign = negate <$ char '-' <|> id <$ char '+' <|> pure id

skipSpace :: Parser ()
skipSpace = Parser \input -> (Just ((), T.dropWhile isSpace input), mempty)

endOfLine :: Parser ()
endOfLine = void (string "\r\n" <|> string "\r" <|> string "\n") <?> "end of line"
