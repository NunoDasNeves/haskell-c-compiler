
-- AST definition
-- No globals (gotta make program a list of statements)

-- Supported types
data AST_Type           = TYPE_INT | TYPE_CHAR deriving (Show)

-- Comparisons == < > <= >=
data AST_Compare        = E | LT | GT | LTE | GTE deriving (Show)

-- Math operations
data AST_Operation      = PLUS | MINUS | MULTIPLY | DIVIDE deriving (Show)

-- A variable has a type and label
data AST_Variable       = AST_Variable AST_Type String deriving (Show)

-- Expressions can have literals, identifiers, function calls, assignments or comparisons
data AST_Expression     = EXPR_INT Int | EXPR_CHAR Char | EXPR_IDENT String | EXPR_CALL String [AST_Expression] | EXPR_ASSIGN AST_Variable AST_Expression | EXPR_COMPARE AST_Expression AST_Compare AST_Expression deriving (Show)

-- Statements can be flow control, raw expressions or variable declarations
data AST_Statement      = WHILE AST_While | IF AST_If | RETURN AST_Expression | EXPR AST_Expression | DECLARE AST_Type AST_Variable deriving (Show)

data AST_While          = AST_While {
                            whileCond :: AST_Expression,
                            whileBody :: [AST_Statement]
                            } deriving (Show)

data AST_If             = AST_If {
                            ifCond :: AST_Expression,
                            ifBody :: [AST_Statement],
                            ifElseIf :: [[AST_Statement]],
                            ifElse :: [AST_Statement]
                            } deriving (Show)

data AST_Function       = AST_Function {
                            retType :: AST_Type,
                            funcName :: String,
                            funcArgs :: [(AST_Type, String)],
                            funcBody :: [AST_Statement]
                            } deriving (Show)

-- A program is a list of functions, one of which is main
data AST_Program        = AST_Program [AST_Function] deriving (Show)

