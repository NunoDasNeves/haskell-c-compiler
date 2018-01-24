
data TokenType = OB | CB | OP | CP | SC | TYPE_INT | RET | IDENTIFIER | INTLIT | UNKNOWN
data Token = Token TokenType String

instance Show Token where
    show (Token _ str) = show str

c_IDENT_FIRST = '_':['a'..'z'] ++ ['A'..'Z']
c_IDENT_CHAR = c_IDENT_FIRST ++ ['0'..'9']

lexIntLit :: Token -> String -> [Token]
lexIntLit t [] = [t]
lexIntLit (Token INTLIT tokStr) (x:xs)
    | x `elem` ['0'..'9']   = lexIntLit (Token INTLIT (tokStr ++ [x])) xs
    | otherwise             = (Token INTLIT tokStr):lexer (x:xs)

lexParseText :: Token -> String -> [Token]
lexParseText t [] = [t]
lexParseText (Token UNKNOWN tokStr) (x:xs)
    | x `elem` c_IDENT_CHAR = lexParseText (Token UNKNOWN (tokStr ++ [x])) xs
    | otherwise             = case tokStr of
                                "return" -> (Token RET "RET"):lexer (x:xs)
                                "int"    -> (Token TYPE_INT "TYPE_INT"):lexer (x:xs)
                                _        -> (Token IDENTIFIER tokStr):lexer (x:xs)

lexer :: String -> [Token]
lexer []      = []
lexer (x:xs)
    | '(' == x = (Token OB "("):lexer xs
    | ')' == x = (Token CB ")"):lexer xs
    | '{' == x = (Token OP "{"):lexer xs
    | '}' == x = (Token CP "}"):lexer xs
    | ';' == x = (Token SC ";"):lexer xs
    | x `elem` ['0'..'9'] = lexIntLit (Token INTLIT [x]) xs
    | x `elem` c_IDENT_FIRST = lexParseText (Token UNKNOWN [x]) xs
    | otherwise = lexer xs


-- AST definition
data AST_Expression     = LITERAL Int deriving (Show)
data AST_Variable       = AST_Variable String deriving (Show)
data AST_Statement      = RETURN AST_Expression | ASSIGN AST_Variable AST_Expression deriving (Show)

data AST_Type           = INT | CHAR deriving (Show)
data AST_Function       = AST_Function {
                            retType :: AST_Type,
                            name :: String,
                            args :: [(AST_Type, String)],
                            body :: [AST_Statement]
                            } deriving (Show)
data AST_Program        = AST_Program [AST_Function] deriving (Show)

-- production rules for recursive descent parsing
-- idk what to do here...
-- gotta return a list of AST_Function
-- gotta move through and return a function, then call that on remaining list, recursively
-- foldl doesn't really work..
-- monads???

--parseExpressions :: [String] -> AST_Expression
--parseExpressions x:xs = 

--parseFunctions :: [String] -> AST_Function
--parseFunctions x:xs

--parse :: [String] -> AST_Program
--parse x = parse_AST_Functions x

