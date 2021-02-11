module Data where

import Ast (Ident)

type Ctx = [(Ident, Value)]

data Value = 
    N Int
  | S String
  | L [Value]
  | R Ctx
  deriving (Eq, Show, Read)
