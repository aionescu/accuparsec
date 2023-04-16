module Data.Accuparsec.Text where

import Control.Applicative(Alternative(..))
import Control.Monad(MonadPlus)
import Data.Char(isDigit, isLetter, ord, isSpace)
import Data.Text(Text)
import Data.Text qualified as T
import GHC.Base(UnliftedType)

showT :: Show a => a -> Text
showT = T.pack . show

data ErrorList :: UnliftedType where
  Nil :: ErrorList
  Snoc :: { expected :: !Text, remainingInput :: Text, rest :: ErrorList } -> ErrorList

data ParseError = ParseError { expected :: Text, remainingInput :: Text }
  deriving stock Show

errorsToList :: ErrorList -> [ParseError]
errorsToList Nil = []
errorsToList (Snoc expected remainingInput rest) = ParseError{..} : errorsToList rest

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
  empty = Parser \input errs -> (# (# (# #) | #), Snoc "empty" input errs #)
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
  fail msg = Parser \input errs -> (# (# (# #) | #), Snoc (T.pack msg) input errs #)
  {-# INLINE fail #-}

runParser :: Parser a -> Text -> Either [ParseError] a
runParser (Parser p) input =
  case p input Nil of
    (# (# | (# a, _ #) #), _ #) -> Right a
    (# _, errs #) -> Left $ errorsToList errs
{-# INLINE runParser #-}

(<?>) :: Parser a -> Text -> Parser a
Parser p <?> lbl = Parser \input errs ->
  case p input errs of
    (# (# (# #) | #), _ #) -> (# (# (# #) | #), Snoc lbl input errs #)
    (# (# | (# a, rest #) #), _ #) -> (# (# | (# a, rest #) #), errs #)
infix 0 <?>
{-# INLINE (<?>) #-}

endOfInput :: Parser ()
endOfInput = Parser \input errs ->
  if T.null input
  then (# (# | (# (), input #) #), errs #)
  else (# (# (# #) | #), Snoc "end of input" input errs #)
{-# INLINE endOfInput #-}

char :: Char -> Parser Char
char c = Parser \input errs ->
  case T.uncons input of
    Just (c', rest) | c == c' -> (# (# | (# c', rest #) #), errs #)
    _ -> (# (# (# #) | #), Snoc (showT c) input errs #)
{-# INLINE char #-}

string :: Text -> Parser Text
string s = Parser \input errs ->
  if s `T.isPrefixOf` input
  then (# (# | (# s, T.drop (T.length s) input #) #), errs #)
  else (# (# (# #) | #), Snoc (showT s) input errs #)
{-# INLINE string #-}

anyChar :: Parser Char
anyChar = Parser \input errs ->
  case T.uncons input of
    Just (c, rest) -> (# (# | (# c, rest #) #), errs #)
    _ -> (# (# (# #) | #), Snoc "any character" input errs #)
{-# INLINE anyChar #-}

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser \input errs ->
  case T.uncons input of
    Just (c, rest) | p c -> (# (# | (# c, rest #) #), errs #)
    _ -> (# (# (# #) | #), Snoc "satisfy" input errs #)
{-# INLINE satisfy #-}

digit :: Parser Char
digit = satisfy isDigit <?> "digit"
{-# INLINE digit #-}

letter :: Parser Char
letter = satisfy isLetter <?> "letter"
{-# INLINE letter #-}

takeWhile :: (Char -> Bool) -> Parser Text
takeWhile p = Parser \input errs ->
  case T.span p input of
    (prefix, rest) -> (# (# | (# prefix, rest #) #), errs #)
{-# INLINE takeWhile #-}

takeWhile1 :: (Char -> Bool) -> Parser Text
takeWhile1 p = Parser \input errs ->
  case T.span p input of
    (prefix, rest) ->
      if T.null prefix
      then (# (# (# #) | #), Snoc "takeWhile1" input errs #)
      else (# (# | (# prefix, rest #) #), errs #)
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
    _ -> p input (Snoc "'+'" input (Snoc "'-'" input errs))
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
    _ -> (# (# (# #) | #), Snoc "end of line" input errs #)
{-# INLINE endOfLine #-}
