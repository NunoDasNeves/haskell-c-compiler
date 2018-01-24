
c_IDENT_FIRST = '_':['a'..'z'] ++ ['A'..'Z']
c_IDENT_CHAR = c_IDENT_FIRST ++ ['0'..'9']

lexIntLit :: String -> String -> [String]
lexIntLit t [] = [t]
lexIntLit tokStr (x:xs)
    | x `elem` ['0'..'9']   = lexIntLit (tokStr ++ [x]) xs
    | otherwise             = tokStr:(lexer (x:xs))

lexParseText :: String -> String -> [String]
lexParseText t [] = [t]
lexParseText tokStr (x:xs)
    | x `elem` c_IDENT_CHAR = lexParseText (tokStr ++ [x]) xs
    | otherwise             = tokStr:lexer (x:xs)

lexer :: String -> [String]
lexer []      = []
lexer (x:xs)
    | x `elem` "(){}[];,"       = [x]:lexer xs
    | x == '0'                  = if head xs == 'x' then lexIntLit [x,'x'] (tail xs) else lexIntLit [x] xs
    | x `elem` ['1'..'9']       = lexIntLit [x] xs
    | x `elem` c_IDENT_FIRST    = lexParseText [x] xs
    | otherwise = lexer xs

