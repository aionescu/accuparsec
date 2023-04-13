module GCL.Parser.Attoparsec where

import Control.Applicative((<**>), (<|>), many, optional)
import Control.Applicative.Combinators(between, skipMany, choice, sepBy, option, skipManyTill)
import Control.Monad.Combinators.Expr(Operator(..), makeExprParser)
import Data.Attoparsec.Text(Parser, (<?>), endOfInput, decimal, digit, letter, signed, skipSpace, anyChar, char, endOfLine, parseOnly, string)
import Data.Function(on)
import Data.Functor(($>))
import Data.List(groupBy, sortOn)
import Data.Ord(Down(..))
import Data.Text(Text)
import Data.Text qualified as T

import GCL.Syntax

ws :: Parser ()
ws = skipSpace *> skipMany (comment *> skipSpace)
  where
    comment = string "--" *> skipManyTill anyChar endOfLine

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
  notReserved . T.pack =<< lexeme ((:) <$> fstChar <*> many sndChar) <?> "Identifier"
  where
    fstChar = letter <|> char '_'
    sndChar = fstChar <|> digit <|> char '\''

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

parse :: Text -> Either String Program
parse = parseOnly $ ws *> program <* endOfInput
