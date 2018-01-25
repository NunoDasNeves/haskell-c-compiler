

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

