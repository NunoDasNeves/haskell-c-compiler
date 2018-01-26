module Parser
(
parse,
AST_Program
) where

import Lexer

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
data AST_Expression     = EXPR_INT Int
                        | EXPR_CHAR Char
                        | EXPR_IDENT String
                        | EXPR_CALL String [AST_Expression]
                        | EXPR_ASSIGN AST_Variable AST_Expression
                        | EXPR_COMPARE AST_Compare AST_Expression AST_Expression
                        | EXPR_OPERATION AST_Operation AST_Expression AST_Expression
                    deriving (Show)

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
                            funcType :: AST_Type,
                            funcName :: String,
                            funcArgs :: [(AST_Type, String)],
                            funcBody :: [AST_Statement]
                            } deriving (Show)

-- A program is a list of functions, one of which is main
data AST_Program        = AST_Program [AST_Function] deriving (Show)

-- Recursive descent parsing
-- Each function returns the unconsumed part of the token list

parseFunction :: [Token] -> (AST_Function, [Token])
parseFunction toks = 
    -- get 3 tokens for (first part of) signature
    let sig = take 3 toks
        afterSig = drop 3 toks
    in
        case sig of
            -- match (type, identifier, "(")
            [(Token TOK_TYPE fType),(Token TOK_IDENT fId),(Token TOK_MISC "(")]  ->
                -- parse args
                let args = fst argOut                   -- [(AST_Type, String)]
                    afterArgs = take 2 (snd argOut)     -- two tokens for ")" "{"
                    bodyToks = drop 2 (snd argOut)      -- the body
                    where argOut = parseArgs afterSig
                in
                    case bodyToks of
                        -- match the close of the args paren ")" and the start of the body "{"
                        [(Token TOK_MISC "("),(Token TOK_MISC "{")] ->
                        let body = fst bodyOut
                            afterBody = head (snd bodyOut)
                            remainingToks = tail (snd bodyOut)
                            where bodyOut = parseExpressions bodyToks
                        in
                            case afterBody of
                                -- match the closing brace "}"
                                (Token TOK_MISC "}") ->
                                    -- finally, return the tuple
                                    (AST_Function {
                                        funcType = parseType t,
                                        funcName = ident,
                                        funcArgs = args,
                                        funcBody = body
                                    }, remainingToks)
                                _ -> error "Function does not terminate properly"
                    _ -> error "Invalid function signature"
            _ -> error "Invalid function signature"

parse :: [Token] -> AST_Program
parse toks = AST_Program (fst ret):parse (snd ret) where ret = parseFunction toks

