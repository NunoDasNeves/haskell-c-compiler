
-- AST definition
-- No globals (gotta make program a list of statements)

-- Supported types
data AST_Type           = INT | CHAR deriving (Show)

-- Comparisons == < > <= >=
data AST_Compare        = E | LT | GT | LTE | GTE deriving (Show)

-- Math operations
data AST_Operation      = PLUS | MINUS | MULTIPLY | DIVIDE

-- A variable has a type and label
data AST_Variable       = AST_Variable AST_Type String deriving (Show)

-- Expressions can have literals, identifiers, function calls, assignments or comparisons
data AST_Expression     = INT Int | CHAR Char | IDENT String | CALL String [AST_Expression] | ASSIGN AST_Variable AST_Expression | COMPARE AST_Expression AST_Compare AST_Expression deriving (Show)

-- Statements can be flow control, raw expressions or variable declarations
data AST_Statement      = WHILE AST_While | IF AST_If | RETURN AST_Expression | EXPR AST_Expression | DECLARE AST_Type AST_Variable deriving (Show)

data AST_While          = AST_While {
                            condition :: AST_Expression,
                            body :: [AST_Statement]
                            }

data AST_If             = AST_If {
                            condition :: AST_Expression,
                            body :: [AST_Statement],
                            elseIfs :: [[AST_Statement]],
                            aElse :: [AST_Statement]
                            }

data AST_Function       = AST_Function {
                            retType :: AST_Type,
                            name :: String,
                            args :: [(AST_Type, String)],
                            body :: [AST_Statement]
                            } deriving (Show)

-- A program is a list of functions, one of which is main
data AST_Program        = AST_Program [AST_Function] deriving (Show)

