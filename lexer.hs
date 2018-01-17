import qualified Data.List

doubleSmallNumber x = if x > 100
                        then x
                        else x*2

fizzbuzz :: [Integer] -> [String]
fizzbuzz xs =   [
                if x `mod` 15 /= 0 then
                    (if x `mod` 5 /= 0 then
                        (if x `mod` 3 /= 0 then
                            show x
                        else "Fizz")
                    else "Buzz")
                else "FizzBuzz"
                | x <- xs
                ]


factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial x = x * factorial (x-1)


fib :: (Integral x) => x -> x
fib x
    | x <= 1    = 1
    | otherwise = fib (x-1) + fib (x-2)

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
        where maxTail = maximum' xs


replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x


take' :: (Num i, Ord i) => i -> [a] -> [a]
take'   n _
        | n <= 0    = []
take'   _ []        = []
take' n (x:xs)      = x:take' (n-1) xs

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _           = []
zip' _ []           = []
zip' (a:as) (b:bs)  = (a,b):zip as bs

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ []      = False
elem' e (x:xs)
    | e == x    = True
    | otherwise = elem' e xs

quicksort :: (Ord a) => [a] -> [a]
quicksort []        = []
quicksort [x]       = [x]
quicksort (x:xs)    = less ++ x:greater
        where less      =   quicksort [s | s <- xs, s <= x]
              greater   =   quicksort [g | g <- xs, g > x]

quicksort2 :: (Ord x) => [x] -> [x]
quicksort2 [] = []
quicksort2 [x] = [x]
quicksort2 (x:xs) = (quicksort2 (filter (\y -> y <= x) xs)) ++ [x] ++ (quicksort2 (filter (\y -> y > x) xs))


filter' :: (x -> Bool) -> [x] -> [x]
filter' _ []    = []
filter' f (x:xs)
    | f x       = x:filter' f xs
    | otherwise = filter' f xs


foldl' :: (x -> x -> x) -> x -> [x] -> x
foldl' _ a []       = a
foldl' f a (x:xs)   = foldl' f (f a x) xs

main :: IO()
main = getContents >>= (\str -> putStrLn $ "hello " ++ str ++ "\n")

main' :: IO()
main' = do {
    str <- getContents;
    putStrLn $ "hello " ++ str ++ "\n";
}


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


search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
        let nlen = length needle
        in foldl (\accum x -> if needle == take nlen x then True else accum) False (Data.List.tails haystack)


data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a    = Node a left right
    | x < a     = Node a (treeInsert x left) right
    | x > a     = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem a EmptyTree = False
treeElem x (Node a left right)
    | x == a    = True
    | x < a     = treeElem x left
    | x > a     = treeElem x right

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

