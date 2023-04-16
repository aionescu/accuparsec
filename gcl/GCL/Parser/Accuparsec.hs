module GCL.Parser.Accuparsec(parse) where

import Control.Applicative((<**>), (<|>), many, optional)
import Control.Applicative.Combinators(between, skipMany, choice, sepBy, option)
import Control.Monad.Combinators.Expr(Operator(..), makeExprParser)
import Data.Accuparsec.Text(Parser, ParseError, (<?>), endOfInput, takeWhile, decimal, signed, skipSpace, endOfLine, runParser, string, satisfy)
import Data.Char(isDigit, isLetter)
import Data.Function(on)
import Data.Functor(($>))
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
  symbol "Int" $> Int
  <|> symbol "Bool" $> Bool
  <|> symbol "Ref" $> Ref

type' :: Parser Type
type' = primType <|> Array <$> btwn "[" "]" primType

exprAtom :: Parser Expr
exprAtom =
  choice
  [ IntLit <$> lexeme (signed decimal)
  , BoolLit True <$ symbol "True"
  , BoolLit False <$ symbol "False"
  , Null <$ symbol "null"
  , Subscript <$> (Var <$> ident) <*> btwn "[" "]" expr
  , ident <**> option Var (symbol "." *> symbol "val" $> GetVal)
  , Length <$> (symbol "#" *> ident)
  , Forall <$> (symbol "forall" *> ident <* symbol ".") <*> expr
  , Exists <$> (symbol "exists" *> ident <* symbol ".") <*> expr
  , btwn "(" ")" expr
  ]

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
  choice
  [ Skip <$ symbol "skip"
  , Assume <$> (symbol "assume" *> expr)
  , Assert <$> (symbol "assert" *> expr)
  , AssignIndex <$> ident <*> (btwn "[" "]" expr <* symbol "=") <*> expr
  , AssignVal <$> ident <* symbol "." <* symbol "val" <* symbol "=" <*> expr
  , AssignNew <$> ident <* symbol "=" <* symbol "new" <*> expr
  , Assign <$> (ident <* symbol "=") <*> expr
  ] <* symbol ";"

stmtCompound :: Parser Stmt
stmtCompound =
  choice
  [ If <$> (symbol "if" *> expr) <*> block <*> option Skip (symbol "else" *> block)
  , While <$> (symbol "while" *> expr) <*> block
  , Let <$> (symbol "let" *> decls) <*> block
  , block
  ] <* optional (symbol ";")

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

parse :: Text -> Either [ParseError] Program
parse = runParser $ ws *> program <* endOfInput
