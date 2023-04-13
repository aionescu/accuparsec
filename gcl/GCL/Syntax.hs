module GCL.Syntax where

import Data.Text(Text)

type Id = Text

data Type
  = Int
  | Bool
  | Ref
  | Array Type
  deriving stock (Eq, Show)

data Op
  = Add
  | Sub
  | Mul
  | Div
  | And
  | Or
  | Implies
  | Eq
  | Neq
  | Lt
  | Lte
  | Gt
  | Gte
  deriving stock (Eq, Enum, Bounded, Show)

data Assoc = L | R | N

associativity :: Op -> Assoc
associativity = \case
  Add -> L
  Sub -> L
  Mul -> L
  Div -> L
  And -> R
  Or -> R
  Implies -> N
  Eq -> N
  Neq -> N
  Lt -> N
  Lte -> N
  Gt -> N
  Gte -> N

precedence :: Op -> Int
precedence = \case
  Mul -> 8
  Div -> 8
  Add -> 7
  Sub -> 7
  Eq -> 6
  Neq -> 6
  Lt -> 5
  Lte -> 5
  Gt -> 5
  Gte -> 5
  And -> 4
  Or -> 3
  Implies -> 2

opSymbol :: Op -> Text
opSymbol = \case
  Add -> "+"
  Sub -> "-"
  Mul -> "*"
  Div -> "/"
  And -> "&&"
  Or -> "||"
  Implies -> "=>"
  Eq -> "=="
  Neq -> "!="
  Lt -> "<"
  Lte -> "<="
  Gt -> ">"
  Gte -> ">="

data Expr
  = IntLit Int
  | BoolLit Bool
  | Null
  | Var Id
  | GetVal Id
  | Length Id
  | Op Op Expr Expr
  | Negate Expr
  | Not Expr
  | Subscript Expr Expr
  | Forall Id Expr
  | Exists Id Expr
  | RepBy Expr Expr Expr
  deriving stock (Eq, Show)

type Pred = Expr

data Decl =
  Decl
  { declName :: Id
  , declType :: Type
  }
  deriving stock (Eq, Show)

data Stmt
  = Skip
  | Assume Expr
  | Assert Expr
  | Assign Id Expr
  | AssignIndex Id Expr Expr
  | AssignNew Id Expr
  | AssignVal Id Expr
  | If Expr Stmt Stmt
  | While Expr Stmt
  | Seq Stmt Stmt
  | Let [Decl] Stmt
  deriving stock (Eq, Show)

data Program =
  Program
  { programName :: Id
  , programInputs :: [Decl]
  , programOutput :: Decl
  , programBody :: Stmt
  }
  deriving stock (Eq, Show)
