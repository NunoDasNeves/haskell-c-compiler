
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

parseStatements :: [String] -> ([AST_Statements], [String])
parseStatements x:xs = 
                | x == "return" = RETURN (parseExpression xs)
                |

"return" <expression> ";"

--parseFunctions :: [String] -> AST_Function
--parseFunctions x:xs

--parse :: [String] -> AST_Program
--parse x = parse_AST_Functions x

