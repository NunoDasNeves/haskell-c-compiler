module Lexer where

data TokenType = TOK_MISC | TOK_TYPE | TOK_IDENT | TOK_KEYWORD | TOK_OPERATOR | TOK_INT | TOK_DOUBLE | TOK_CHAR | TOK_STRING deriving (Show)
data Token = Token {
                    tokenType :: TokenType,
                    valueString :: String
                    } deriving (Show)

--instance Show Token where
--    show (Token _ str) = show str

c_MISC = "(){}[];,"

c_IDENT_FIRST = '_':['a'..'z'] ++ ['A'..'Z']
c_IDENT_CHAR = c_IDENT_FIRST ++ ['0'..'9']
c_KEYWORDS = ["if","while","return"]--"else","__syscall"
c_TYPES = ["int", "char"]
c_OPERATORS = "=><*/&!+-"--"^%~|.?:"


-- strings!
lexParseString :: String -> String -> [Token]
lexParseString t [] = error "Lexer couldn't parse string literal"
lexParseString tokStr (x:xs)
    | x == '\\'             = lexParseString (tokStr ++ [head xs]) (tail xs)
    | x == '\"'             = (Token TOK_STRING tokStr):lexer xs
    | otherwise             = lexParseString (tokStr ++ [x]) xs


-- operators
lexParseOperator :: String -> String -> [Token]
lexParseOperator t [] = error "Lexer couldn't parse operator"
lexParseOperator tokStr (x:xs)
    | x `elem` c_OPERATORS  = lexParseOperator (tokStr ++ [x]) xs
    | otherwise             = (Token TOK_OPERATOR tokStr):lexer (x:xs)

-- chars - give this a string starting with the char, as in "a'"
lexParseChar :: String -> [Token]
lexParseChar [] = error "Lexer couldn't parse char literal"
lexParseChar [t] = error "Lexer couldn't parse char literal"
lexParseChar (x:xs) = if head xs == '\''
                        then (Token TOK_CHAR [x]):(lexer $ tail xs)
                        else error "Lexer couldn't parse char literal"

-- integers, including hex and octal, and doubles
-- TODO doubles
lexParseNum :: String -> String -> [Token]
lexParseNum t [] = error "Lexer couldn't parse numeric literal"
lexParseNum tokStr (x:xs)
    | x `elem` ['0'..'9']   = lexParseNum (tokStr ++ [x]) xs
    | otherwise             = (Token TOK_INT tokStr):lexer (x:xs)

-- text tokens - identifiers, types, keywords
lexParseText :: String -> String -> [Token]
lexParseText t [] = error "Lexer couldn't parse identifier"
lexParseText tokStr (x:xs)
    | x `elem` c_IDENT_CHAR     = lexParseText (tokStr ++ [x]) xs
    | tokStr `elem` c_KEYWORDS  = (Token TOK_KEYWORD tokStr):lexer (x:xs)
    | tokStr `elem` c_TYPES     = (Token TOK_TYPE tokStr):lexer (x:xs)
    | otherwise                 = (Token TOK_IDENT tokStr):lexer (x:xs)

-- main lexer function
lexer :: String -> [Token]
lexer [] = []
lexer (x:xs)
    | x `elem` c_MISC           = (Token TOK_MISC [x]):lexer xs
    | x == '"'                  = lexParseString "" xs
    | x == '\''                 = lexParseChar xs
    | x == '0'                  = if head xs == 'x' then lexParseNum "0x" (tail xs) else lexParseNum "0" xs
    | x `elem` ['1'..'9']       = lexParseNum [x] xs
    | x `elem` c_IDENT_FIRST    = lexParseText [x] xs
    | x `elem` c_OPERATORS      = lexParseOperator [x] xs
    | otherwise = lexer xs

