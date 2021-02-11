module Ast where

type Template = [Frag]

data Frag =
    TLit String
  | TOutput Exp
  | TAssign Var Exp
  | TIf Exp Template Template
  | TFor Var Exp Template
  | TCapture Var Template
  deriving (Eq, Show, Read)

data Exp =
    EVar Var
  | ENum Int
  | EField Exp Field
  | EPlus Exp Exp
  | ELeq Exp Exp
  deriving (Eq, Show, Read)

type Var = Ident
type Field = Ident

type Ident = String
