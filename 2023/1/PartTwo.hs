module PartTwo where

import PartOne (aggregate, calculate)

main :: IO ()
main = do
  input <- parse <$> readFile "document.txt"
  print $ aggregate calculate input

parse :: String -> [String]
parse = map (foldr f "") . lines

f :: Char -> String -> String
f c acc = 
  case (c : acc) of
    ('o' : 'n' : 'e'             : _) -> c : '1' : acc
    ('t' : 'w' : 'o'             : _) -> c : '2' : acc
    ('t' : 'h' : 'r' : 'e' : 'e' : _) -> c : '3' : acc
    ('f' : 'o' : 'u' : 'r'       : _) -> c : '4' : acc
    ('f' : 'i' : 'v' : 'e'       : _) -> c : '5' : acc
    ('s' : 'i' : 'x'             : _) -> c : '6' : acc
    ('s' : 'e' : 'v' : 'e' : 'n' : _) -> c : '7' : acc
    ('e' : 'i' : 'g' : 'h' : 't' : _) -> c : '8' : acc
    ('n' : 'i' : 'n' : 'e'       : _) -> c : '9' : acc
    _                                 -> c : acc
