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
                            funcArgs :: [AST_Variable],
                            funcBody :: [AST_Statement]
                            } deriving (Show)

-- A program is a list of functions, one of which is main
data AST_Program        = AST_Program [AST_Function] deriving (Show)

-- Recursive descent parsing
-- Each function returns the unconsumed part of the token list

-- types are a single string
parseType :: String -> AST_Type
parseType s = case s of
                "int"   -> TYPE_INT
                "char"  -> TYPE_CHAR
--                _       -> error "Invalid type \"" ++ s ++ "\""

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

-- TODO implement properly
parseBody :: [Token] -> ([AST_Statement], [Token])
parseBody toks = ([], toks)

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
                let args = fst argOut                   -- [(AST_Type, String)]
                    afterArgs = take 2 (snd argOut)     -- two tokens for ")" "{"
                    bodyToks = drop 2 (snd argOut)      -- the body
                in
                    case afterArgs of
                        -- match the close of the args paren ")" and the start of the body "{"
                        [(Token TOK_MISC ")"),(Token TOK_MISC "{")] ->
                            let body = fst bodyOut
                                afterBody = head (snd bodyOut)
                                remainingToks = tail (snd bodyOut)
                            in
                                case afterBody of

                                    -- match the closing brace "}"
                                    (Token TOK_MISC "}") ->
                                        -- finally, return the tuple
                                        (AST_Function {
                                            funcType = parseType fType,
                                            funcName = fId,
                                            funcArgs = args,
                                            funcBody = body
                                        }, remainingToks)

--                                    _ -> error "Function does not terminate properly"

                            where bodyOut = parseBody bodyToks

--                        _ -> tError "Invalid function signature"

                where argOut = parseArgs afterSig

--            _ -> error "Invalid function signature"

parseFunctions :: [Token] -> [AST_Function]
parseFunctions [] = []
parseFunctions toks = (fst ret):parseFunctions (snd ret) where ret = parseFunction toks

parse :: [Token] -> AST_Program
parse toks = AST_Program (parseFunctions toks)

