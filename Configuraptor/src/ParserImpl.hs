module ParserImpl where

import Absyn

import Control.Monad
import Data.Either
import Data.Char
import Text.ParserCombinators.Parsec
import Text.Parsec.Error

-- GRAMMAR 
-------------
-- Database ::= Declz
-- Declz ::= ϵ
-- | Decl Declz
-- Decl ::= ‘resource’ RNames ‘.’
-- | ‘component’ CName ‘:’ Clauses ‘.’
-- RNames ::= RName
-- | RName ‘,’ RNames
-- Clauses ::= Clause
-- | Clause ‘;’ Clauses
-- Clause ::= ‘provides’ RSpec
-- | ‘uses’ RSpec
-- | ‘requires’ RSpec

-- RSpec ::= RSpec1 RSpec'
-- RSpec' ::=  '|' RSpec1 RSpec'
-- | empty
-- RSpec1 ::= RSpec2 RSpec1'
-- RSpec1' ::= ',' RSpec2 Rspec1'
-- | empty
-- RSpec2 ::= num Rspec2
-- | '(' RSpec ')'    
-- | RName

-- RName ::= name
-- CName ::= name

whiteSpace1 :: Parser ()
whiteSpace1 = choice [spaceChar *> whiteSpace, comment *> whiteSpace]
    where
        comment = do
            char '{' 
            viable
        spaceChar = void $ many1 (choice (map char [' ', '\n', '\t']))
        viable = (nonComment *> viable) <|> (comment *> viable) <|> char '}'
        nonComment = satisfy (\a -> isPrint a && a /= '{' && a /= '}')

whiteSpace :: Parser ()
whiteSpace = choice [spaceChar *> whiteSpace, comment *> whiteSpace, return ()]
     where
        comment = do
            char '{' 
            viable
        spaceChar = void $ many1 (choice (map char [' ', '\n', '\t']))
        viable = (nonComment *> viable) <|> (comment *> viable) <|> char '}'
        nonComment = satisfy (\a -> isPrint a && a /= '{' && a /= '}')

nameP :: Parser String
nameP = do
    fc <- firstChar
    rest <- many nonFirstChar
    whiteSpace
    if length rest < 32 then
        return (fc:rest) else
            fail "name too long"
    where
        firstChar = satisfy (\a -> isLetter a || isDigit a)
        nonFirstChar = satisfy (\a -> isDigit a || isLetter a) <|>
            do
                char '-'
                satisfy (\a -> isDigit a || isLetter a)

numP :: Parser Int
numP = do
    number <- many1 digit
    whiteSpace
    if length number < 7 then
        return (read number) else
            fail "number too big"

rspecP :: Parser RSpec
rspecP = rspec1P >>= rspecP'

rspecP' :: RSpec -> Parser RSpec
rspecP' r = ((char '|' >> whiteSpace) *> (RSOr r <$> rspec1P) >>= rspecP')
    <|> return r

rspec1P :: Parser RSpec
rspec1P = rspec2P >>= rspec1P'

rspec1P' :: RSpec -> Parser RSpec
rspec1P' r = ((char ',' >> whiteSpace1) *> (RSAnd r <$> rspec2P) >>= rspec1P')
    <|> return r

rspec2P :: Parser RSpec
rspec2P = RSNum <$> numP <*> rspec2P
    <|> RSRes <$> nameP
    <|> between (char '(' >> whiteSpace) (char ')' >> whiteSpace) rspecP

clauseP :: Parser Clause
clauseP = do 
        provide <- (string "provides" >> whiteSpace1) *> rspecP
        return (CKProvides, provide)
    <|> do
        use <- (string "uses" >> whiteSpace1) *> rspecP
        return (CKUses, use)
    <|> do 
        require <- (string "requires" >> whiteSpace1) *> rspecP
        return (CKRequires, require)

compP :: Parser IComp
compP = IC <$> nameP <*> ((char ':' >> whiteSpace) *> sepBy1 clauseP 
    (char ';' >> whiteSpace))

declP :: Parser [Either RName IComp]
declP = do 
        string "resource" >> whiteSpace1
        sepBy1 (Left <$> nameP) (char ',' >> whiteSpace)
    <|> do
        string "component" >> whiteSpace1
        comp <- Right <$> compP
        return [comp]

databaseP :: Parser IDB
databaseP = do
    list <- endBy declP (char '.' >> whiteSpace)
    eof
    return (partitionEithers (concat list))

parseString :: String -> Either String IDB 
parseString s = case parse databaseP "" s of
    Left _ -> Left "Syntax error in input string"
    Right idb -> Right idb
