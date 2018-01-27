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

type AST_Label          = String

-- A variable (declaration in a statement or arg list) has a type and label
data AST_Variable       = AST_Variable AST_Type AST_Label deriving (Show)

-- Expressions can have literals, identifiers, function calls, assignments or comparisons
data AST_Expression     = EXPR_INT Int                                                  -- <int literal>
                        | EXPR_CHAR Char                                                -- <char literal>
                        | EXPR_IDENT AST_Label                                          -- <label>
                        | EXPR_CALL AST_Label [AST_Expression]                          -- <label>(<args>)
                        | EXPR_ASSIGN AST_Expression AST_Expression                     -- <expression> = <expression> (expression on left side must be ident)
                        | EXPR_COMPARE AST_Compare AST_Expression AST_Expression        -- <expression> <comparison operator> <expression>
                        | EXPR_OPERATION AST_Operation AST_Expression AST_Expression    -- <expression> <math operator> <expression>
                    deriving (Show)

-- Statements can be flow control, raw expressions or variable declarations
data AST_Statement      = STAT_RETURN AST_Expression                                         -- return <expression>;
                        | STAT_EXPR AST_Expression                                           -- <expression>;
                        | STAT_DECLARE AST_Variable                                          -- <type> <label>;
                    deriving (Show) -- | WHILE AST_While | IF AST_If deriving (Show)

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
            (Token TOK_OPERATOR op) | op `elem` ["==",">=","<=",">","<","+","-","*","/"] ->
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
                        in (EXPR_COMPARE E expr nextExpr, toks'')
                    -- math
                    "+" ->
                        let (nextExpr, toks'') = parseExpression $ tail toks'
                        in (EXPR_OPERATION PLUS expr nextExpr, toks'')

                    _ -> error "Invalid operator, or operator not supported yet"
                        
            _ -> error $ "Invalid token " ++ show (head toks') ++ " following expression"
-- some rough examples of what this should parse
-- x
-- 1
-- 'a'
-- x = 1
-- <expr assign <expr var> <expr int>
-- x = 1 + 1
-- <expr assign <expr var> <expr operation <expr int> <expr int>>>
-- x == (1 + 1)
-- <expr compare <expr var> <expr operation <expr int> <expr int>>>
-- 1 - x
-- <expr operation <expr int> <expr var>>
-- x >= foo()
-- foo(1+1, x)
-- <expr call <ident> [<expr operation + <expr int 1> <expr int 1>>, <expr var>]>
-- x = 1 + 2 + 3
-- <expr assign <expr var x> <expr operation + <expr int 1> <expr operation + <expr int 2> <expr int 3>>>>   

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
                    _ -> error "Unknown keyword"
        -- TODO identifiers!
        _ -> error "Unknown statement type"

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


parseFunctions :: [Token] -> [AST_Function]
parseFunctions [] = []
parseFunctions toks = (fst ret):parseFunctions (snd ret) where ret = parseFunction toks

parse :: [Token] -> AST_Program
parse toks = AST_Program (parseFunctions toks)

