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
                        | TYPE_PTR AST_Type
                        | TYPE_ARRAY AST_Type Int    -- array size
                    deriving (Show)

-- Binary operators
type AST_Bin_Operator   = String --OP_ASSIGN     -- =
--                        | OP_EE         -- ==
--                        | OP_NE         -- !=
--                        | OP_LT         -- <
--                        | OP_GT         -- >
--                        | OP_LTE        -- <=
--                        | OP_GTE        -- >=
--                        | OP_PLUS       -- +
--                        | OP_MINUS      -- -
--                        | OP_TIMES      -- *
--                        | OP_DIVIDE     -- /
--                    deriving (Show)

data AST_Un_Operator   = OP_NOT deriving (Show)

-- Literals in the AST
data AST_Literal        = LIT_INT Int | LIT_CHAR Char | LIT_CHAR_PTR String deriving (Show)

-- Expressions can have literals, identifiers, function calls, assignments or comparisons
data AST_Expression     = EXPR_LITERAL AST_Literal                                      -- <int literal>
                        | EXPR_IDENT AST_Label                                          -- <label>
                        -- | EXPR_ASSIGN AST_Expression AST_Expression                      -- <expression> = <expression> (left side must be ident or deref)
                        -- | EXPR_COMPARE AST_Compare AST_Expression AST_Expression         -- <expression> <comparison operator> <expression>
                        | EXPR_BIN_OPERATION AST_Bin_Operator AST_Expression AST_Expression -- <expression> <math operator> <expression>
                        | EXPR_CALL AST_Label [AST_Expression]                          -- <label>(<args>) each arg is another expression, comma delimited
                        | EXPR_PTR_INDEX AST_Label AST_Expression                       -- <expression>[expression]
                        | EXPR_PTR_DEREF AST_Expression                                 -- *<expression>
                        | EXPR_PTR_GET AST_Label                                        -- &<label>
                        | EXPR_UN_OPERATION AST_Un_Operator AST_Expression              -- !<expression>
                    deriving (Show)

-- Statements can be flow control, raw expressions or variable declarations
data AST_Statement      = STAT_RETURN AST_Expression                                    -- return <expression>;
                        | STAT_RETURN_VOID                                              -- return;
                        | STAT_EXPR AST_Expression                                      -- <label> = <expression>; (only assignment allowed)
                        | STAT_DECLARE AST_Type AST_Label                               -- <type> <label>;
                        | STAT_DECLARE_ASSIGN AST_Type AST_Label AST_Expression         -- <type> <label> = <expression>;
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
                            funcArgs :: [(AST_Type, AST_Label)],
                            funcBody :: [AST_Statement]
                            } deriving (Show)

-- A program is a list of functions, one of which is main
data AST_Program        = AST_Program [AST_Function] deriving (Show)

-- Recursive descent parsing
-- Each function returns the unconsumed part of the token list

-- types are a single string
-- TODO move to parseDeclaration
parseType :: String -> AST_Type
parseType s =
    case s of
        "int"   -> TYPE_INT
        "char"  -> TYPE_CHAR
        _       -> error $ "Invalid type \"" ++ s ++ "\""

-- recursively look for *'s or [<number>]'s for nesting derived pointer types
-- takes the base type as arg
parsePtrOrArray :: AST_Type -> [Token] -> (AST_Type, [Token])
parsePtrOrArray _ [] = error "No tokens left to parse!"
parsePtrOrArray childType (tok:toks) =
    case tok of
        -- sequence of *s
        (Token TOK_OPERATOR "*") ->
            case childType of
                (TYPE_ARRAY _ _) ->
                    error "Invalid type declaration"
                _ ->
                    parsePtrOrArray (TYPE_PTR childType) toks
        -- sequence of [<number>]s
        (Token TOK_MISC "[") ->
            let nextTwo = take 2 toks
                toks' = drop 2 toks
            in
                case nextTwo of
                    [(Token TOK_INT arrSizeStr),(Token TOK_MISC "]")] ->
                        let arrSize = read arrSizeStr
                        in
                            if arrSize > 0
                            then parsePtrOrArray (TYPE_ARRAY childType arrSize) toks'
                            else error "Invalid array size"
                    _ -> error "Invalid array declaration"
        -- something else; end of this string of *s or []s
        _ -> (childType, tok:toks)

-- parse a variable declaration, including complete type and identifier
parseDeclaration :: [Token] -> ((AST_Type, AST_Label), [Token])
parseDeclaration [] = error "No tokens left to parse!"
parseDeclaration ((Token TOK_TYPE typeStr):toks) =
    let (firstTypePart, tok':toks') = parsePtrOrArray (parseType typeStr) toks
    in
        case tok' of
            (Token TOK_IDENT vLabel) ->
                let (finalType, toks'') = parsePtrOrArray firstTypePart toks'
                in
                    ((finalType, vLabel), toks'')
            _-> error "Invalid type"
parseDeclaration toks = error "Invalid type"


-- function arguments
parseArgs :: [Token] -> ([(AST_Type, AST_Label)], [Token])
parseArgs [] = error "No tokens left to parse!"
parseArgs toks =
    case head toks of
        (Token TOK_MISC ")") ->
            ([], toks)
        _ ->
            let (arg, toks') = parseDeclaration toks
            in
                case head toks' of
                    (Token TOK_MISC ",") ->
                        let (argList, toks'') = parseArgs (tail toks')
                        in
                            (arg:argList, toks'')
                    _ -> ([arg], toks')


-- simply parse an expression starting with "(" (a group of expressions in brackets)
parseExprGroup :: [Token] -> (AST_Expression, [Token])
parseExprGroup []   = error "No tokens left to parse"
parseExprGroup ((Token TOK_MISC delim):toks) =
    if delim == "("
    then
        let (innerExpr, toks') = parseExpression toks
        in
            case head toks' of
                (Token TOK_MISC ")") ->
                    (innerExpr, tail toks')
                _ -> error "Invalid expression, no terminating )"
    else
        let (innerExpr, toks') = parseExpression toks
        in
            case head toks' of
                (Token TOK_MISC "]") ->
                    (innerExpr, tail toks')
                _ -> error "Invalid expression, no terminating ]"
parseExprGroup _ = error "Cannot parse expression group"


-- unary expressions have highest priority; this function parses them in a hard-coded manner
parseUnExpr :: [Token] -> (AST_Expression, [Token])
parseUnExpr (tok:toks) =
    case tok of
        -- if a paren is opened, get the inner expression first, then continue
        (Token TOK_MISC "(") ->
            parseExprGroup (tok:toks)
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
                -- dereference with an index
                (Token TOK_MISC "[") ->
                    let (indexExpr, afterIndex) = parseExprGroup toks
                    in ((EXPR_PTR_INDEX ident indexExpr), afterIndex)
                -- if it's not an opening paren or assignment, we'll check the next character later.
                _ -> (EXPR_IDENT ident, toks)
        -- or a literal
        (Token TOK_INT int) ->
            (EXPR_LITERAL (LIT_INT (read int)), toks)
        (Token TOK_CHAR char) ->
            (EXPR_LITERAL (LIT_CHAR (head char)), toks)
        (Token TOK_STRING str) ->
            (EXPR_LITERAL (LIT_CHAR_PTR str), toks)
        -- or a unary operator
        (Token TOK_OPERATOR "!") ->
            let (expr, toks') = parseUnExpr toks
            in (EXPR_UN_OPERATION OP_NOT expr, toks')
        (Token TOK_OPERATOR "*") ->
            let (expr, toks') = parseUnExpr toks
            in (EXPR_PTR_DEREF expr, toks')
        -- has to be followed by a lone identifier
        (Token TOK_OPERATOR "&") ->
            case head toks of
                (Token TOK_IDENT ident) ->
                    (EXPR_PTR_GET ident, tail toks)
                _ -> error "Invalid operand of & pointer operator"
        _ -> error $ "Invalid expression starting with " ++ show tok


-- get precedence of a binary operator
getOpPrio :: String -> Int
getOpPrio x
    -- | x `elem` ["(","[","!","&"]    = 7
    | x `elem` ["*","/"]            = 6 -- "%"
    | x `elem` ["+","-"]            = 5
    | x `elem` ["<=",">=","<",">"]  = 4
    | x `elem` ["==","!="]          = 3
    | x `elem` ["="]                = 2
    | otherwise                     = error "Unrecognized operator"


-- parses a sequence of binary expressions, given the lefthand size of the first
parseBinExpr :: AST_Expression -> Int -> [Token] -> (AST_Expression, [Token])
-- if we have an operator, parse with precedence
parseBinExpr lhs minPrio ((Token TOK_OPERATOR op):toks)
    | nextPrio >= minPrio   =   let (rhs, toks') = parseUnExpr toks
                                in
                                    let (expr, toks'') = parseRhsExpr rhs nextPrio toks'
                                    in (EXPR_BIN_OPERATION op lhs expr, toks'')
    | otherwise             = (lhs, (Token TOK_OPERATOR op):toks)
    where nextPrio = getOpPrio op
-- otherwise just return lhs
parseBinExpr lhs _ toks = (lhs, toks)

parseRhsExpr :: AST_Expression -> Int -> [Token] -> (AST_Expression, [Token])
parseRhsExpr rhs prio ((Token TOK_OPERATOR op):toks)
    | nextPrio > prio   =   let (expr, toks') = parseBinExpr rhs nextPrio ((Token TOK_OPERATOR op):toks)
                            in parseRhsExpr expr nextPrio toks'
    | otherwise         = (rhs, (Token TOK_OPERATOR op):toks)
    where nextPrio = getOpPrio op
parseRhsExpr rhs _ toks = (rhs, toks)

-- 1 * 2 + 3
-- pBE("1", 0, "* 2 + 3")
-- nextPrio = 4


-- Parse an expression
parseExpression :: [Token] -> (AST_Expression, [Token])
parseExpression []   = error "No tokens left to parse"
parseExpression toks =
    -- get lhs in case there are ()'s or unary expressions
    let (lhs, toks') = parseUnExpr toks
    in
        case head toks' of
            -- We're done; end of expression
            (Token TOK_MISC aChar) | aChar `elem` [")",";",",","]"]  ->
                (lhs, toks')
            -- Wait, there's a binary operator! so it's actually a larger expression!
            (Token TOK_OPERATOR op) | op `elem` ["==",">=","<=",">","<","+","-","*","/"] ->
                -- delegate to our operator precedence parser
                parseBinExpr lhs 0 toks'
            -- Check assignment by hand - lowest priority
            (Token TOK_OPERATOR "=") ->
                case lhs of
                    (EXPR_IDENT _) ->
                        let (rhs, toks'') = parseExpression (tail toks')
                        in (EXPR_BIN_OPERATION "=" lhs rhs, toks'')
                    _ -> error $ "Cannot assign to " ++ show lhs

            _ -> error $ "Invalid token " ++ show (head toks') ++ " following expression"


-- parse a semicolon-terminated statement
parseStatement :: [Token] -> (AST_Statement, [Token])
parseStatement []   = error "No tokens left to parse"
parseStatement toks =
    case head toks of
        (Token TOK_TYPE _) ->
            let ((vType, vName), toks') = parseDeclaration toks
            in
                case head toks' of
                    -- just declaration
                    (Token TOK_MISC ";") ->
                        (STAT_DECLARE vType vName, tail toks')
                    -- declaration + assignment
                    (Token TOK_OPERATOR "=") ->
                        let (expr, toks'') = parseExpression (tail toks')
                        in
                            case head toks'' of
                                (Token TOK_MISC ";") ->
                                    (STAT_DECLARE_ASSIGN vType vName expr, tail toks'')
                                _ -> error "Failed to parse declaration"
                    _ -> error "Failed to parse declaration"
        (Token TOK_KEYWORD keyword) ->
            let toks' = tail toks
            in
                case keyword of
                    "return" ->
                        case head toks' of
                            -- empty return statement
                            (Token TOK_MISC ";") -> 
                                (STAT_RETURN_VOID, tail toks')
                            -- return some kinda expression
                            _ ->
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
                    (EXPR_BIN_OPERATION "=" _ _) ->
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

-- function definitions are parsed like a regular variable declaration, followed by args and a body
parseFunction :: [Token] -> (AST_Function, [Token])
parseFunction [] = error "No tokens left to parse"
parseFunction toks =
    let ((fType, fName), afterDecl) = parseDeclaration toks
    in
        case head afterDecl of
            -- match (type, identifier, "(")
            (Token TOK_MISC "(")  ->
                -- parse args
                let
                    (args, afterArgs) = parseArgs (tail afterDecl)
                in
                    let
                        nextTwo = take 2 afterArgs
                        bodyToks = drop 2 afterArgs
                    in
                        case nextTwo of
                            -- match the close of the args paren ")" and the start of the body "{"
                            [(Token TOK_MISC ")"),(Token TOK_MISC "{")] ->
                                let (body, afterBody) = parseBody bodyToks
                                in
                                    case head afterBody of
                                        -- match the closing brace "}"
                                        (Token TOK_MISC "}") ->
                                            -- finally, return the tuple
                                            (AST_Function {
                                                funcType = fType,
                                                funcName = fName,
                                                funcArgs = args,
                                                funcBody = body
                                            }, tail afterBody)
                                        _ -> error "Invalid function termination"
                            _ -> error "Invalid characters after function arguments"
            _ -> error "Invalid function argument list"


-- parse a list of functions
parseFunctions :: [Token] -> [AST_Function]
parseFunctions [] = []
parseFunctions toks = (fst ret):parseFunctions (snd ret) where ret = parseFunction toks

-- the top-level parse function!
parse :: [Token] -> AST_Program
parse toks = AST_Program (parseFunctions toks)

