module Parser
(
parse,
AST_Program
) where

import Lexer

-- AST definition

type AST_Label          = String

-- Supported types
data AST_Type           = TYPE_INT
                        | TYPE_CHAR
                        | TYPE_INT_PTR
                        | TYPE_CHAR_PTR
                        | TYPE_INT_ARRAY Int    -- array size
                        | TYPE_CHAR_ARRAY Int   -- array size
                    deriving (Show)

-- Comparisons == < > <= >=
data AST_Compare        = COMP_E | COMP_LT | COMP_GT | COMP_LTE | COMP_GTE deriving (Show)

-- Math and logical operators
data AST_Operator      = OP_PLUS | OP_MINUS | OP_MULTIPLY | OP_DIVIDE | OP_NOT deriving (Show)

-- A variable (declaration in a statement or arg list) has a type and label
data AST_Variable       = AST_Variable AST_Type AST_Label deriving (Show)

data AST_Literal        = LIT_INT Int | LIT_CHAR Char | LIT_CHAR_PTR String deriving (Show)

-- Expressions can have literals, identifiers, function calls, assignments or comparisons
data AST_Expression     = EXPR_LITERAL AST_Literal                                      -- <int literal>
                        | EXPR_IDENT AST_Label                                          -- <label>
                        | EXPR_CALL AST_Label [AST_Expression]                          -- <label>(<args>)
                        | EXPR_ASSIGN AST_Expression AST_Expression                     -- <expression> = <expression> (left side must be ident or deref)
                        | EXPR_COMPARE AST_Compare AST_Expression AST_Expression        -- <expression> <comparison operator> <expression>
                        | EXPR_BIN_OPERATION AST_Operator AST_Expression AST_Expression -- <expression> <math operator> <expression>
                        | EXPR_UN_OPERATION AST_Operator AST_Expression                 -- !<expression>, ~<expression>
                        | EXPR_PTR_DEREF AST_Expression                                 -- *<expression> (allow dereference anything?)
                        | EXPR_PTR_GET AST_Label                                        -- &<label>
                    deriving (Show)

-- Statements can be flow control, raw expressions or variable declarations
data AST_Statement      = STAT_RETURN AST_Expression                                         -- return <expression>;
                        | STAT_EXPR AST_Expression                                           -- <label> = <expression>; (only assignment allowed)
                        | STAT_DECLARE AST_Variable                                          -- <type> <label>;
                        | STAT_DECLARE_ASSIGN AST_Variable AST_Expression                    -- <type> <label> = <expression>;
                        | STAT_WHILE AST_While
                        | STAT_IF AST_If
                    deriving (Show)

data AST_While          = AST_While {
                            whileCond :: AST_Expression,
                            whileBody :: [AST_Statement]
                            } deriving (Show)

data AST_If             = AST_If {
                            ifCond :: AST_Expression,
                            ifBody :: [AST_Statement]--,
                            --ifElseIf :: [[AST_Statement]],
                            --ifElse :: [AST_Statement]
                            } deriving (Show)

data AST_Function       = AST_Function {
                            funcType :: AST_Type,
                            funcName :: AST_Label,
                            funcArgs :: [AST_Variable],
                            funcBody :: [AST_Statement]
                            } deriving (Show)

-- A program is a list of functions, one of which is main
data AST_Program        = AST_Program [AST_Function] deriving (Show)

-- Recursive descent parsing
-- Each function returns the unconsumed part of the token list

-- types are a single string
parseType :: String -> AST_Type
parseType s =
    case s of
        "int"   -> TYPE_INT
        "char"  -> TYPE_CHAR
        _       -> error $ "Invalid type \"" ++ s ++ "\""

-- function arguments must be (type, identifier) followed by a "," or ")"
parseArgs :: [Token] -> ([AST_Variable], [Token])
parseArgs [] = error "No tokens left to parse!"
parseArgs toks =
    let arg = take 2 toks
        afterArg = drop 2 toks
    in
        case arg of
            [(Token TOK_TYPE tStr),(Token TOK_IDENT iStr)] ->
                case head afterArg of

                    -- if "," recursively get rest of args
                    (Token TOK_MISC ",") -> 
                        let remainingArgs   = fst argOut
                            remainingTokens = snd argOut
                        in
                            ((AST_Variable (parseType tStr) iStr):remainingArgs, remainingTokens)
                            where argOut = parseArgs (tail afterArg) -- skip the ","

                    -- otherwise, we're done and can return a single element list with the final arg
                    _ -> 
                        ([(AST_Variable (parseType tStr) iStr)], afterArg)    -- keep the ")"

            _ -> ([], toks)

parseExpression :: [Token] -> (AST_Expression, [Token])
parseExpression []   = error "No tokens left to parse"
parseExpression (tok:toks) =
    -- for whatever reason this needs to be indented like this apparently
    let (expr, toks') = case tok of
                            -- if a paren is opened, get the inner expression first, then continue
                            (Token TOK_MISC "(") ->
                                let (innerExpr, afterExpr) = parseExpression toks
                                in
                                    case head afterExpr of
                                        (Token TOK_MISC ")") ->
                                            (innerExpr, tail afterExpr)
                                        _ -> error "Invalid expression, no terminating )"

                            -- otherwise, it may be an identifier..
                            (Token TOK_IDENT ident) ->
                                case head toks of
                                    -- function call
                                    (Token TOK_MISC "(") ->
                                        let (argList, afterCall) = ([], tail toks)--TODO parseExpressions tail toks
                                        in
                                            case head afterCall of
                                                (Token TOK_MISC ")") ->
                                                    ((EXPR_CALL ident argList), tail afterCall)
                                                _ -> error "Invalid function call, no terminating )"
                                    -- if it's not an opening paren or assignment, we'll check the next character later.
                                    _ -> (EXPR_IDENT ident, toks)
                            -- convert integers or characters to expressions
                            (Token TOK_INT int) ->
                                (EXPR_INT (read int), toks)
                            (Token TOK_CHAR char) ->
                                (EXPR_CHAR (head char), toks)
                            _ -> error $ "Invalid expression starting with " ++ show tok
    in
        case head toks' of
            -- We're done; end of expression
            (Token TOK_MISC aChar) | aChar `elem` [")",";",","]  ->
                (expr, toks')
            -- Wait, there's an operator! so it's actually a larger expression!
            (Token TOK_OPERATOR op) | op `elem` ["==",">=","<=",">","<","+","-","*","/","="] ->
                case op of
                    -- assignment; first expression must be identifier
                    "=" ->
                        case expr of
                            (EXPR_IDENT _) ->
                                let (nextExpr, toks'') = parseExpression $ tail toks'
                                in (EXPR_ASSIGN expr nextExpr, toks'')
                            _ -> error $ "Cannot assign to " ++ show expr
                    -- comparison
                    "==" ->
                        let (nextExpr, toks'') = parseExpression $ tail toks'
                        in (EXPR_COMPARE COMP_E expr nextExpr, toks'')
                    -- math
                    "+" ->
                        let (nextExpr, toks'') = parseExpression $ tail toks'
                        in (EXPR_BIN_OPERATION OP_PLUS expr nextExpr, toks'')

                    _ -> error "Invalid operator, or operator not supported yet"
            _ -> error $ "Invalid token " ++ show (head toks') ++ " following expression"

-- parse a semicolon-terminated statement
parseStatement :: [Token] -> (AST_Statement, [Token])
parseStatement []   = error "No tokens left to parse"
parseStatement toks =
    case head toks of
        (Token TOK_TYPE _) ->
            let decl = take 3 toks
                toks' = drop 3 toks
            in
                case decl of
                    [(Token TOK_TYPE vType),(Token TOK_IDENT vId),(Token TOK_MISC ";")] ->
                        ((STAT_DECLARE (AST_Variable (parseType vType) vId)), toks')
                    _ -> error "Failed to parse declaration"
        (Token TOK_KEYWORD keyword) ->
            let toks' = tail toks
            in
                case keyword of
                    "return" ->
                        let (expr, toks'') = parseExpression toks'
                        in
                            case head toks'' of
                                (Token TOK_MISC ";") ->
                                    (STAT_RETURN expr, tail toks'')
                                _ -> error "Failed to parse return statement"
                    x | x `elem` ["if", "while"] ->
                        case head toks' of
                            (Token TOK_MISC "(") ->
                                let (expr, toks'') = parseExpression $ tail toks'
                                in
                                    let nextTwo = take 2 toks''
                                        toks''' = drop 2 toks''
                                    in
                                        case nextTwo of
                                            [(Token TOK_MISC ")"),(Token TOK_MISC "{")] ->
                                                let (statements, toks'''') = parseBody toks'''
                                                in
                                                    case head toks'''' of
                                                        (Token TOK_MISC "}") ->
                                                            if keyword == "if"
                                                            then
                                                                (STAT_IF (AST_If expr statements), tail toks'''')
                                                            else
                                                                (STAT_WHILE (AST_While expr statements), tail toks'''')
                                                        _ -> error "Invalid if/while statement"
                                            _ -> error "Invalid tokens after if/while expression"
                            _ -> error "Invalid if/while statement"
                    _ -> error "Unknown keyword"
        (Token TOK_IDENT ident) ->
            let (expr, toks') = parseExpression toks
            in
                case expr of
                    (EXPR_ASSIGN _ _) ->
                        case head toks' of
                            (Token TOK_MISC ";") ->
                                (STAT_EXPR expr, tail toks')
                            _ -> error "Failed to parse assignment statement"
                    _ -> error "Statement expressions can only be assignments"
        _ -> error $ "Unknown statement type" ++ show (head toks)

-- parse a semicolon-delimited list of statements
parseBody :: [Token] -> ([AST_Statement], [Token])
parseBody []    = error "No tokens left to parse"
parseBody toks  = 
    case head toks of
        (Token TOK_MISC "}") ->
            ([], toks)
        _ ->
            let (statement, toks') = parseStatement toks
            in
                let (statementList, toks'') = parseBody toks'
                in
                    (statement:statementList, toks'')

-- function definitions have a single word type, followd by an identifier, args, and a body
parseFunction :: [Token] -> (AST_Function, [Token])
parseFunction [] = error "No tokens left to parse"
parseFunction toks = 
    -- get 3 tokens for (first part of) signature
    let sig = take 3 toks
        afterSig = drop 3 toks
    in
        case sig of
            -- match (type, identifier, "(")
            [(Token TOK_TYPE fType),(Token TOK_IDENT fId),(Token TOK_MISC "(")]  ->

                -- parse args
                let 
                    (args, toks') = parseArgs afterSig
                in
                    let
                        afterArgs = take 2 toks'
                        bodyToks = drop 2 toks'
                    in
                        case afterArgs of
                            -- match the close of the args paren ")" and the start of the body "{"
                            [(Token TOK_MISC ")"),(Token TOK_MISC "{")] ->
                                let (body, toks'') = parseBody bodyToks
                                in
                                    case head toks'' of

                                        -- match the closing brace "}"
                                        (Token TOK_MISC "}") ->
                                            -- finally, return the tuple
                                            (AST_Function {
                                                funcType = parseType fType,
                                                funcName = fId,
                                                funcArgs = args,
                                                funcBody = body
                                            }, tail toks'')


-- parse a list of functions
parseFunctions :: [Token] -> [AST_Function]
parseFunctions [] = []
parseFunctions toks = (fst ret):parseFunctions (snd ret) where ret = parseFunction toks

-- the top-level parse function!
parse :: [Token] -> AST_Program
parse toks = AST_Program (parseFunctions toks)

