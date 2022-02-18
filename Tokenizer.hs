module Tokenizer where
import Data.Typeable

main :: IO()
main = do
 putStrLn $ testOutput tokenize "tokenize" test_string_1 expected_value_1
 putStrLn $ testOutput tokenize "tokenize" test_string_2 expected_value_2
 putStrLn $ testOutput tokenize "tokenize" test_string_3 expected_value_3
 putStrLn $ testOutput tokenize "tokenize" test_string_4 expected_value_4
 putStrLn $ testOutput tokenize "tokenize" test_string_5 expected_value_5
 putStrLn $ testOutput tokenize "tokenize" test_string_6 expected_value_6
 putStrLn $ testOutput tokenize "tokenize" test_string_7 expected_value_7
 where test_string_1 = "3-10*25/SoG"
       test_string_2 = "(2+ x ) *\t40"
       test_string_3 = "     35 + 10 -   \n  (4 / MonthlyResult )       "
       test_string_4 = "5++ */tomorrow(-3"
       test_string_5 = "@"
       test_string_6 = "hello 1234 ++) @ ()() hello"
       test_string_7 = ""
       expected_value_1 = [("3",Value 3),("-",Subtract),("10",Value 10),("*",Multiply),("25",Value 25),("/",Divide),("SoG",Id)]
       expected_value_2 = [("(",LeftPar),("2",Value 2),("+",Add),("x",Id),(")",RightPar),("*",Multiply),("40",Value 40)]
       expected_value_3 = [("35",Value 35),("+",Add),("10",Value 10),("-",Subtract),("(",LeftPar),("4",Value 4),("/",Divide),("MonthlyResult",Id),(")",RightPar)]
       expected_value_4 = [("5",Value 5),("+",Add),("+",Add),("*",Multiply),("/",Divide),("tomorrow",Id),("(",LeftPar),("-",Subtract),("3",Value 3)]
       expected_value_5 = []
       expected_value_6 = []
       expected_value_7 = []


split :: [Char] -> [[Char]]
split[] = []
split (x:xs)
 | isLetter x = word : (split.eatWord) xs
 | isDigit x = number : (split.eatInt) xs
 | isChar x = [x] : split xs
 | otherwise = split xs
 where word = takeWhile (\x -> elem x(['A' .. 'Z'] ++ ['a' .. 'z'])) (x:xs)
       number = takeWhile(\x -> elem x (['0'..'9'])) (x:xs)
       eatWord = dropWhile (\x -> elem x (['A' .. 'Z'] ++ ['a' .. 'z']))
       eatInt = dropWhile (\x -> elem x(['0'..'9']))
       isLetter x = elem x (['A' .. 'Z'] ++ ['a' .. 'z'])
       isDigit x = elem x ['0' .. '9']
       isChar x = elem x ['(',')','+','-','*','/']


makeToken :: String -> Token
makeToken s@(x:xs) 
 | elem x ['0'.. '9'] = Value (read s :: Int)
 | "(" == s = LeftPar
 | ")" == s = RightPar
 | "+" == s = Add
 | "-" == s = Subtract
 | "*" == s = Multiply
 | "/" == s = Divide
 | elem x (['A' .. 'Z'] ++ ['a' .. 'z']) = Id
 | otherwise = Undefined


tokenize :: String -> [(String, Token)]
tokenize [] = []
tokenize s = zipWith(,) (split s) (map makeToken (split s))



testOutput :: (Show a, Eq a, Show b, Eq b) => (a -> b) -> String -> a ->  b -> String
testOutput f name input expected = concat [ name, " ",  show input, " should be ", show expected, " -->" ,
                                                                                                                let result = f input in
                                                                                                                 if result == expected
                                                                                                                 then "PASS"
                                                                                                                 else concat["FAIL (actual: ", show result, ")"]]
data Token = Id | Value Int
    | Add | Subtract | Multiply | Divide 
    | LeftPar | RightPar | Undefined
    deriving (Show, Eq)