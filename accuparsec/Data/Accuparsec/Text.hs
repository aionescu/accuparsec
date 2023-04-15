module Data.Accuparsec.Text where

import Control.Applicative(Alternative(..))
import Control.Monad(MonadPlus)
import Data.Char(isDigit, isLetter, ord, isSpace)
import Data.Text(Text)
import Data.Text qualified as T

import Data.SList

showT :: Show a => a -> Text
showT = T.pack . show

data ParseError
  = Error { expected :: !Text, remainingInput :: Text }
  | Label { label :: !Text, remainingInput :: Text }
  deriving stock (Show)

type ErrorList = SList ParseError

newtype Parser a =
  Parser
  { unParser :: Text -> ErrorList -> (# (# (# #) | (# a, Text #) #), ErrorList #)
  }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser \input errs ->
    case p input errs of
      (# (# | (# a, rest #) #), errs' #) -> (# (# | (# f a, rest #) #), errs' #)
      (# _, errs' #) -> (# (# (# #) | #), errs' #)
  {-# INLINE fmap #-}

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser \input errs -> (# (# | (# a, input #) #), errs #)
  {-# INLINE pure #-}

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  Parser f <*> Parser a = Parser \input errs ->
    case f input errs of
      (# (# | (# f', rest #) #), errs' #) ->
        case a rest errs' of
          (# (# | (# a', rest' #) #), errs'' #) -> (# (# | (# f' a', rest' #) #), errs'' #)
          (# _, errs'' #) -> (# (# (# #) | #), errs'' #)
      (# _, errs' #) -> (# (# (# #) | #), errs' #)
  {-# INLINE (<*>) #-}

instance Alternative Parser where
  empty :: Parser a
  empty = Parser \input errs -> errs `seq` (# (# (# #) | #), errs :! Label "empty" input #)
  {-# INLINE empty #-}

  (<|>) :: Parser a -> Parser a -> Parser a
  Parser a <|> Parser b = Parser \input errs ->
    case a input errs of
      (# (# (# #) | #), errs' #) -> b input errs'
      result -> result
  {-# INLINE (<|>) #-}

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser a >>= f = Parser \input errs ->
    case a input errs of
      (# (# | (# a', rest #) #), errs' #) -> unParser (f a') rest errs'
      (# _, errs' #) -> (# (# (# #) | #), errs' #)
  {-# INLINE (>>=) #-}

instance MonadPlus Parser

instance MonadFail Parser where
  fail :: String -> Parser a
  fail msg = Parser \input errs -> errs `seq` (# (# (# #) | #), errs :! Label (T.pack msg) input #)
  {-# INLINE fail #-}

runParser :: Parser a -> Text -> Either ErrorList a
runParser (Parser p) input =
  case p input Nil of
    (# (# | (# a, _ #) #), _ #) -> Right a
    (# _, !errs #) -> Left errs
{-# INLINE runParser #-}

(<?>) :: Parser a -> Text -> Parser a
Parser p <?> lbl = Parser \input errs ->
  case p input errs of
    (# (# (# #) | #), _ #) -> (# (# (# #) | #), errs :! Label lbl input #)
    result -> result
infix 0 <?>
{-# INLINE (<?>) #-}

endOfInput :: Parser ()
endOfInput = Parser \input errs ->
  if T.null input
  then (# (# | (# (), input #) #), errs #)
  else errs `seq` (# (# (# #) | #), errs :! Error "end of input" input #)
{-# INLINE endOfInput #-}

char :: Char -> Parser Char
char c = Parser \input errs ->
  case T.uncons input of
    Just (c', rest) | c == c' -> (# (# | (# c', rest #) #), errs #)
    _ -> errs `seq` (# (# (# #) | #), errs :! Error (showT c) input #)
{-# INLINE char #-}

string :: Text -> Parser Text
string s = Parser \input errs ->
  if s `T.isPrefixOf` input
  then (# (# | (# s, T.drop (T.length s) input #) #), errs #)
  else errs `seq` (# (# (# #) | #), errs :! Error (showT s) input #)
{-# INLINE string #-}

anyChar :: Parser Char
anyChar = Parser \input errs ->
  case T.uncons input of
    Just (c, rest) -> (# (# | (# c, rest #) #), errs #)
    _ -> errs `seq` (# (# (# #) | #), errs :! Error "any character" input #)
{-# INLINE anyChar #-}

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser \input errs ->
  case T.uncons input of
    Just (c, rest) | p c -> (# (# | (# c, rest #) #), errs #)
    _ -> errs `seq` (# (# (# #) | #), errs :! Label "satisfy" input #)
{-# INLINE satisfy #-}

digit :: Parser Char
digit = satisfy isDigit <?> "digit"
{-# INLINE digit #-}

letter :: Parser Char
letter = satisfy isLetter <?> "letter"
{-# INLINE letter #-}

takeWhile1 :: (Char -> Bool) -> Parser Text
takeWhile1 p = Parser \input errs ->
  case T.span p input of
    (prefix, rest) ->
      if T.null prefix
      then (# (# (# #) | #), errs :! Label "takeWhile1" input #)
      else errs `seq` (# (# | (# prefix, rest #) #), errs #)
{-# INLINE takeWhile1 #-}

decimal :: Integral a => Parser a
decimal = T.foldl' step 0 <$> takeWhile1 isDigit <?> "decimal"
  where
    step a c = a * 10 + fromIntegral (ord c - 48)
{-# SPECIALISE decimal :: Parser Int #-}
{-# SPECIALISE decimal :: Parser Word #-}
{-# SPECIALISE decimal :: Parser Integer #-}
{-# INLINE decimal #-}

signed :: Num a => Parser a -> Parser a
signed (Parser p) = Parser \input errs ->
  case T.uncons input of
    Just ('-', rest) ->
      case p rest errs of
        (# (# | (# a, rest' #) #), errs' #) -> (# (# | (# negate a, rest' #) #), errs' #)
        result -> result
    Just ('+', rest) -> p rest errs
    _ -> errs `seq` p input (errs :! Error "'-'" input :! Error "'+'" input)
{-# SPECIALISE signed :: Parser Int -> Parser Int #-}
{-# SPECIALISE signed :: Parser Word -> Parser Word #-}
{-# SPECIALISE signed :: Parser Integer -> Parser Integer #-}
{-# INLINE signed #-}

skipSpace :: Parser ()
skipSpace = Parser \input errs -> (# (# | (# (), T.dropWhile isSpace input #) #), errs #)
{-# INLINE skipSpace #-}

endOfLine :: Parser ()
endOfLine = Parser \input errs ->
  case T.uncons input of
    Just ('\n', rest) -> (# (# | (# (), rest #) #), errs #)
    Just ('\r', rest) ->
      case T.uncons rest of
        Just ('\n', rest') -> (# (# | (# (), rest' #) #), errs #)
        _ -> (# (# | (# (), rest #) #), errs #)
    _ -> errs `seq` (# (# (# #) | #), errs :! Error "end of line" input #)
{-# INLINE endOfLine #-}
