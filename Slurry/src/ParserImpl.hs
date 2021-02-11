module ParserImpl where

import Ast

import Text.Parsec.Char
import Text.ParserCombinators.Parsec

-- GRAMMAR ------------------

-- template = frags
-- frags = frag frags | literal frag frags | literal | empty
-- frag = output | assignment | conditional | iteration | capture
-- output = "{{" exp "}}"
-- assignment = "{%" "assign" var "=" exp "%}"
-- conditional = "{%" "if" exp "%}" template condRest "{%" "endif" "%}"
-- condRest = "{%" "elsif" exp "%}" template condRest | "{%" "else" "%}" template | empty
-- template = "{%" "for" var "in" exp "%}" template "{%" "endfor" "%}"
-- capture = "{%" "capture" var "%}" template "{%" "endcapture" "%}"

-- exp = exp1 exp'
-- exp' = "<=" exp1 exp' | empty
-- exp1 = exp2 exp1'
-- exp1' = "+" exp2 exp1' | empty
-- exp2 = exp3 exp2'
-- exp2' = "." field | empty
-- exp3 =  "(" exp ")" | var | numeral

-- var = ident
-- literal = ident
-- field = ident

-----------------------------
-- whitespace ---------------

whiteSpace1 :: Parser ()
whiteSpace1 = skipMany1 (choice [space, endOfLine])

whiteSpace :: Parser ()
whiteSpace = skipMany (choice [space, endOfLine])

-----------------------------
-- template parser ----------
numP :: Parser Int 
numP = do
    whiteSpace
    number <- many1 digit
    return (read number)

identP ::Parser String
identP = do
    whiteSpace
    first <- letter
    rest <- many (choice [letter, digit, char '_'])
    return (first:rest)

-----
expP :: Parser Exp
expP = exp1P >>= expP'

expP' :: Exp -> Parser Exp
expP' e = try (do
    whiteSpace
    string "<="
    whiteSpace
    expression <- exp1P >>= expP'
    return (ELeq e expression))
    <|>
    return e

exp1P :: Parser Exp
exp1P = exp2P >>= exp1P'

exp1P' :: Exp -> Parser Exp
exp1P' e = try (do
    whiteSpace
    char '+'
    whiteSpace
    expression <- exp2P >>= exp1P'
    return (EPlus e expression))
    <|> 
    return e

exp2P :: Parser Exp 
exp2P = exp3P >>= exp2P'

exp2P' :: Exp -> Parser Exp 
exp2P' e = try (do
    whiteSpace
    char '.'
    whiteSpace
    (EField e <$> identP) >>= exp2P')
    <|>
    return e

exp3P :: Parser Exp
exp3P = EVar <$> identP
    <|> ENum <$> numP
    <|> between (char '(') (char ')') (do
        whiteSpace
        expression <- expP
        whiteSpace
        return expression)

-----
outputP :: Parser Frag
outputP = between (string "{{") (string "}}") (do 
    whiteSpace
    expression <- expP
    whiteSpace
    return (TOutput expression))
    
-----
assignmentP :: Parser Frag
assignmentP = between (string "{%") (string "%}") (do
    whiteSpace
    string "assign"
    whiteSpace1
    variable <- identP
    whiteSpace
    char '='
    whiteSpace
    expression <- expP
    whiteSpace
    return (TAssign variable expression))

-----
condRestP :: Parser Template
condRestP = 
    try (do
        conditional <- between (string "{%") (string "%}") (do
            whiteSpace
            string "elsif"
            whiteSpace1
            expression <- expP
            whiteSpace
            return expression)
        template <- templateP
        rest <- condRestP
        return [TIf conditional template rest])
    <|>
    try (do 
        between (string "{%") (string "%}") (do
            whiteSpace
            string "else"
            whiteSpace)
        templateP)
    <|>
    try (do  -- for when there's an else but no template
        between (string "{%") (string "%}") (do
            whiteSpace
            string "else"
            whiteSpace)
        lookAhead (between (string "{%") (string "%}") (do
                whiteSpace
                string "endif"
                whiteSpace))
        return [])
    <|> 
    try (do  -- for when there's no else
        lookAhead (between (string "{%") (string "%}") (do
                whiteSpace
                string "endif"
                whiteSpace))
        return [])

conditionalP :: Parser Frag
conditionalP = do
    conditional <- between (string "{%") (string "%}") (do
        whiteSpace
        string "if"
        whiteSpace1
        expression <- expP
        whiteSpace
        return expression)
    template <- templateP
    rest <- condRestP
    between (string "{%") (string "%}") (do
        whiteSpace
        string "endif"
        whiteSpace)
    return (TIf conditional template rest)

-----
iterationP :: Parser Frag
iterationP = do
    loop <- between (string "{%") (string "%}") (do
        whiteSpace
        string "for"
        whiteSpace1
        variable <- identP
        whiteSpace1
        string "in"
        whiteSpace1
        expression <- expP
        whiteSpace
        return (TFor variable expression))
    template <- templateP
    between (string "{%") (string "%}") (do
        whiteSpace
        string "endfor"
        whiteSpace)
    return (loop template)

captureP :: Parser Frag
captureP = do
    capture <- between (string "{%") (string "%}") (do
        whiteSpace
        string "capture"
        whiteSpace1
        variable <- identP
        whiteSpace
        return variable)
    template <- templateP
    between (string "{%") (string "%}") (do
        whiteSpace
        string "endcapture"
        whiteSpace)
    return (TCapture capture template)

-----
logicP :: Parser Frag
logicP = try assignmentP
    <|> try conditionalP
    <|> try iterationP
    <|> try captureP

-----
litsP :: Parser Char
litsP = 
    try (do
        character <- char '{'
        char ' '
        return character)
    <|>
    endOfLine
    <|>
    satisfy (\a -> (a /= '{'))

literalP :: Parser Frag
literalP = TLit <$> many1 (try litsP)

-----
fragP :: Parser Frag
fragP = try outputP
    <|> try logicP
    <|> literalP

-----
templateP :: Parser Template
templateP = many fragP

slurryP :: Parser Template
slurryP = do
    template <- templateP
    eof
    return template

-----------------------------
-- module -------------------

type ParseErr = String

parseString :: String -> Either ParseErr Template
parseString s = case parse slurryP "" s of
    Left e -> Left (show e)
    Right template -> Right template

-----------------------------