module GCL.Parser.Attoparsec(parse, parseWithRemainingInput) where

import Control.Applicative((<|>), many)
import Control.Applicative.Combinators(between, skipMany, sepBy, option)
import Control.Monad.Combinators.Expr(Operator(..), makeExprParser)
import Data.Attoparsec.Text(Parser, (<?>), endOfInput, takeWhile, decimal, signed, skipSpace, endOfLine, parseOnly, string, satisfy)
import Data.Attoparsec.Text qualified as A
import Data.Char(isDigit, isLetter)
import Data.Function(on)
import Data.List(groupBy, sortOn)
import Data.Ord(Down(..))
import Data.Text(Text)
import Data.Text qualified as T
import Prelude hiding (takeWhile)

import GCL.Syntax

ws :: Parser ()
ws = skipSpace *> skipMany (comment *> skipSpace) <?> "whitespace or comment"
  where
    comment = string "--" *> takeWhile (\c -> c /= '\r' && c /= '\n') *> endOfLine

lexeme :: Parser a -> Parser a
lexeme p = p <* ws

symbol :: Text -> Parser Text
symbol s = string s <* ws

btwn :: Text -> Text -> Parser a -> Parser a
btwn = between `on` symbol

reserved :: [Text]
reserved =
  [ "Int", "Bool", "Ref"
  , "True", "False"
  , "skip", "assume", "assert"
  , "if", "else", "while", "let"
  , "forall", "exists"
  , "null", "val", "new", "H"
  ]

ident :: Parser Text
ident =
  notReserved =<< lexeme (T.cons <$> fstChar <*> sndChar) <?> "Identifier"
  where
    fstChar = satisfy \c -> isLetter c || c == '_'
    sndChar = takeWhile \c -> isLetter c || isDigit c || c == '_' || c == '\''
    notReserved i
      | i `elem` reserved = fail $ "Reserved identifier " <> show i
      | otherwise = pure i

primType :: Parser Type
primType =
  Int <$ symbol "Int"
  <|> Bool <$ symbol "Bool"
  <|> Ref <$ symbol "Ref"

type' :: Parser Type
type' = primType <|> Array <$> btwn "[" "]" primType

exprAtom :: Parser Expr
exprAtom =
  IntLit <$> lexeme (signed decimal)
  <|> BoolLit True <$ symbol "True"
  <|> BoolLit False <$ symbol "False"
  <|> Null <$ symbol "null"
  <|> Subscript <$> (Var <$> ident) <*> btwn "[" "]" expr
  <|> (\a f -> f a) <$> ident <*> option Var (GetVal <$ symbol "." <* symbol "val")
  <|> Length <$> (symbol "#" *> ident)
  <|> Forall <$> (symbol "forall" *> ident <* symbol ".") <*> expr
  <|> Exists <$> (symbol "exists" *> ident <* symbol ".") <*> expr
  <|> btwn "(" ")" expr

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ Prefix $ Negate <$ symbol "-"
  , Prefix $ Not <$ symbol "!"
  ]
  : ((mkOp <$>) <$> grouped [minBound ..])
  where
    grouped =
      fmap (sortOn $ Down . length . show)
      . groupBy ((==) `on` precedence)
      . sortOn (Down . precedence)

    mkOp op = infix' (associativity op) $ Op op <$ symbol (opSymbol op)
    infix' = \case
      L -> InfixL
      R -> InfixR
      N -> InfixN

expr :: Parser Expr
expr = makeExprParser exprAtom operatorTable

stmtSimple :: Parser Stmt
stmtSimple =
  (Skip <$ symbol "skip"
  <|> Assume <$> (symbol "assume" *> expr)
  <|> Assert <$> (symbol "assert" *> expr)
  <|> AssignIndex <$> ident <*> (btwn "[" "]" expr <* symbol "=") <*> expr
  <|> AssignVal <$> ident <* symbol "." <* symbol "val" <* symbol "=" <*> expr
  <|> AssignNew <$> ident <* symbol "=" <* symbol "new" <*> expr
  <|> Assign <$> (ident <* symbol "=") <*> expr
  ) <* symbol ";"

stmtCompound :: Parser Stmt
stmtCompound =
  If <$> (symbol "if" *> expr) <*> block <*> option Skip (symbol "else" *> block)
  <|> While <$> (symbol "while" *> expr) <*> block
  <|> Let <$> (symbol "let" *> decls) <*> block
  <|> block

stmt :: Parser Stmt
stmt = stmtSimple <|> stmtCompound

block :: Parser Stmt
block = btwn "{" "}" $ foldr Seq Skip <$> many stmt

decl :: Parser Decl
decl = Decl <$> (ident <* symbol ":") <*> type'

decls :: Parser [Decl]
decls = decl `sepBy` symbol ","

program :: Parser Program
program =
  Program
  <$> ident
  <*> btwn "(" ")" decls
  <*> (symbol "->" *> decl)
  <*> block

parseWithRemainingInput :: Text -> Maybe Int
parseWithRemainingInput input =
  case parseOnly' (ws *> program <* endOfInput) input of
    A.Fail rest _ _ -> Just $ T.length rest
    _ -> Nothing

parse :: Text -> Either String Program
parse = parseOnly $ ws *> program <* endOfInput

parseOnly' :: Parser a -> Text -> A.Result a
parseOnly' p input =
  case A.parse p input of
    A.Partial f -> f ""
    r -> r
