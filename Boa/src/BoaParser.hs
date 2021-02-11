-- Skeleton file for Boa Parser.
module BoaParser
  ( ParseError
  , parseString
  ) where

import BoaAST

import Control.Monad
import Data.Char
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec

-- GRAMMAR
-- stmts = stmt stmts'
-- stmts' = ";" stmt stmts' | empty
-- stmt = ident "=" expr | expr
-- expr = "not" expr' | expr' | stringConst
-- expr' = exprArith exprRel
--       | exprArith "==" bool
--       | exprArith "!=" bool
--       | bool "==" exprArith
--       | bool "!=" exprArith
--       | bool
--       | exprList
-- exprRel = "==" exprArith | "!=" exprArith 
--         | "<" exprArith | "<=" exprArith 
--         | ">" exprArith | ">=" exprArith
--         | "in" exprList | "not in" exprList
--         | empty
-- exprArith = term exprArith'
-- exprArith' = "+" term exprArith' 
--            | "-" term exprArith'
--            | empty
-- term = fact term'
-- term' = "*" fact term'
--       | "//" fact term'
--       | "%" fact term'
--       | empty
-- fact = numbConst | ident | "(" exprArith ")" | ident "(" exprs ")"
-- exprList = "[" exprs "]" | "[" expr forQual quals "]" 
-- exprs = empty | expr exprs'
-- exprs' = "," expr exprs' | empty
-- quals = forQual quals | ifQual quals | empty
-- forQual = "for" ident "in" expr
-- ifQual = "if" expr


whiteSpace1 :: Parser ()
whiteSpace1 = choice [spaceChar *> whiteSpace
  ,lineComment *> whiteSpace]
  where
    lineComment = try (char '#') *> manyTill anyChar (void (char '\n') <|> eof)
    spaceChar = void $ many1 (choice (map char [' ', '\n', '\t']))

whiteSpace :: Parser ()
whiteSpace = choice [spaceChar *> whiteSpace
  ,lineComment *> whiteSpace
  ,return ()]
  where
    lineComment = try (char '#') *> manyTill anyChar (void (char '\n') <|> eof)
    spaceChar = void $ many1 (choice (map char [' ', '\n', '\t']))

stmtsP :: Parser Program
stmtsP = do
  parsed <- sepBy1 (whiteSpace >> stmtP) (whiteSpace >> char ';')
  eof
  return parsed

stmtP :: Parser Stmt
stmtP = try (SDef <$> (identP <* char '=') <*> (whiteSpace >> exprP)) <|> try (SExp <$> exprP)

exprP :: Parser Exp
exprP =
  try (string "not" *> (whiteSpace1 >> (Not <$> exprP))) <|>
  try (string "not" *> (whiteSpace >> between (char '(') (char ')') (Not <$> exprP))) <|> 
  exprP' <|>
  Const . StringVal <$> stringP

exprP' :: Parser Exp
exprP' =
  try ((Call <$> identP) <*> between (char '(') (char ')') (sepBy exprP (char ','))) <|>
  try (exprArithP >>= exprRelP) <|>
  try exprListP <|>
  try (Oper Eq <$> (exprArithP <* string "==") <*> (Const <$> boolP)) <|>
  try (Oper Eq <$> ((Const <$> boolP) <* string "==") <*> exprArithP) <|>
  try (Not <$> (Oper Eq <$> (exprArithP <* string "!=") <*> (Const <$> boolP))) <|>
  try (Not <$> (Oper Eq <$> ((Const <$> boolP) <* string "!=") <*> exprArithP)) <|>
  try (Const <$> boolP) <|>
  try exprListP

exprRelP :: Exp -> Parser Exp
exprRelP e =
  try (string "==" *> (Oper Eq e <$> exprArithP)) <|> try (string "!=" *> (Not <$> (Oper Eq e <$> exprArithP))) <|>
  try (string "<" *> (Oper Less e <$> exprArithP)) <|>
  try (string ">" *> (Oper Greater e <$> exprArithP)) <|>
  try (string "<=" *> (Not <$> (Oper Greater e <$> exprArithP))) <|>
  try (string ">=" *> (Not <$> (Oper Less e <$> exprArithP))) <|>
  try (string "in" *> (whiteSpace1 >> (Oper In e <$> exprListP))) <|>
  try (string "in" *> (between (char '(') (char ')') (Oper In e <$> exprListP))) <|>
  try ((do
    string "not"
    whiteSpace1 
    string "in") *> (whiteSpace1 >> (Not <$> (Oper In e <$> exprListP)))) <|>
  try ((do 
    string "not"
    whiteSpace1 
    string "in") *> (between (char '(') (char ')') (Not <$> (Oper In e <$> exprListP)))) <|>
  return e

exprArithP :: Parser Exp
exprArithP = try (termP >>= exprArithP')

exprArithP' :: Exp -> Parser Exp
exprArithP' e =
  (char '+' *> (Oper Plus e <$> termP) >>= exprArithP') <|>
  (char '-' *> (Oper Minus e <$> termP) >>= exprArithP') <|>
  return e

termP :: Parser Exp
termP = factP >>= termP'

termP' :: Exp -> Parser Exp
termP' e =
  (char '*' *> (Oper Times e <$> factP) >>= termP') <|>
  (string "//" *> (Oper Div e <$> factP) >>= termP') <|>
  (char '%' *> (Oper Mod e <$> factP) >>= termP') <|>
  return e

factP :: Parser Exp
factP =
  between (char '(') (char ')') exprP <|> Const . IntVal <$> intP <|> Var <$> identP

exprListP :: Parser Exp
exprListP = do
  l <- try (List <$> between (char '[') (whiteSpace >> char ']') (sepBy exprP (char ','))) <|>
    try (between
      (char '[')
      (char ']')
      ((Compr <$> (whiteSpace >> exprP)) <*> ((:) <$> (whiteSpace >> forQualP) <*> many qualsP))) <|>
    try (Var <$> identP)
  whiteSpace
  return l

qualsP :: Parser Qual
qualsP = try forQualP <|> ifQualP

forQualP :: Parser Qual
forQualP = QFor <$> (string "for" *> (whiteSpace1 >> identP)) <*> (string "in" *> (whiteSpace1 >> exprP))

ifQualP :: Parser Qual
ifQualP = QIf <$> (string "if" *> (whiteSpace1 >> exprP))

boolP :: Parser Value
boolP = do
  b <- TrueVal <$ (whiteSpace >> string "True") <|> FalseVal <$ (whiteSpace >> string "False") <|> NoneVal <$ (whiteSpace >> string "None")
  whiteSpace
  return b

intP :: Parser Int
intP = try (whiteSpace >> pPos) <|> (whiteSpace >> pNeg)
  where
    pPos = do
      s <- zero <|> posNumbers
      return $ read s
      where
        zero = do
          z <- string "0"
          notFollowedBy (char 'x')
          notFollowedBy digit
          whiteSpace
          return z
        posNumbers = do
          p <- oneOf "123456789"
          a <- many digit
          whiteSpace
          return (p : a)
    pNeg = do
      char '-'
      n <- pPos
      return (-n)

reserved :: [String]
reserved = ["None", "False", "True", "for", "if", "in", "not"]       

stringP :: Parser String
stringP = do
  s <- between (char '\'') (char '\'') (many chars)
  whiteSpace
  return $ concat s
  where
    chars = try conditions <|> codes 
    conditions = do
      c <- satisfy (\a -> isPrint a && a /= '\\' && a /= '\'')
      return [c]
    codes = do
      char '\\'
      c <- choice $ map string ["\'", "\\", "n", "\n"]
      return (case c of 
        "n" -> "\n"
        "\n" -> ""
        _ -> c)

identP :: Parser String
identP = do
  whiteSpace
  fc <- firstChar
  rest <- try (many nonFirstChar)
  whiteSpace
  let name = fc : rest
  if name `notElem` reserved
    then return name
    else fail "variable not allowed to be a reserved word"
  where
    firstChar = satisfy (\a -> isLetter a || a == '_')
    nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

parseString :: String -> Either ParseError Program
parseString = parse stmtsP ""
