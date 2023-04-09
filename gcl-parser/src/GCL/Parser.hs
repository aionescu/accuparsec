module GCL.Parser(parse) where

import Control.Applicative((<**>))
import Control.Monad.Combinators.Expr(Operator(..), makeExprParser)
import Data.Bifunctor(first)
import Data.Function(on)
import Data.Functor(($>))
import Data.List(groupBy, sortOn)
import Data.Ord(Down(..))
import Data.Text(Text)
import Data.Text qualified as T
import Data.Void(Void)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import GCL.Syntax

type Parser = Parsec Void Text

lineComm :: Parser ()
lineComm = L.skipLineComment "--"

blockComm :: Parser ()
blockComm = L.skipBlockCommentNested "{-" "-}"

sc :: Parser ()
sc = L.space space1 lineComm blockComm

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

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
  try (notReserved . T.pack =<< lexeme ((:) <$> fstChar <*> many sndChar) <?> "Identifier")
  where
    fstChar = letterChar <|> char '_'
    sndChar = alphaNumChar <|> char '_' <|> char '\''

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
  [ IntLit <$> lexeme L.decimal
  , BoolLit True <$ symbol "True"
  , BoolLit False <$ symbol "False"
  , Null <$ symbol "null"
  , try $ Subscript <$> (Var <$> ident) <*> btwn "[" "]" expr
  , ident <**> option Var (try $ symbol "." *> symbol "val" $> GetVal)
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
  , try $ AssignIndex <$> ident <*> (btwn "[" "]" expr <* symbol "=") <*> expr
  , try $ AssignVal <$> ident <* symbol "." <* symbol "val" <* symbol "=" <*> expr
  , try $ AssignNew <$> ident <* symbol "=" <* symbol "new" <*> expr
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
decls = decl `sepEndBy` symbol ","

program :: Parser Program
program =
  Program
  <$> ident
  <*> btwn "(" ")" decls
  <*> (symbol "->" *> decl)
  <*> block
  <*> pure 0

parse :: FilePath -> Text -> Either Text Program
parse path code =
  first (("Parser error:\n" <>) . T.pack . errorBundlePretty)
  $ runParser (optional sc *> program <* eof) path code
