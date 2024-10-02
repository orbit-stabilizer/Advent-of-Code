module PartTwo where

import Data.List
import PartOne (aggregate, calculate)

main :: IO ()
main = do
  input <- parse <$> readFile "document.txt"
  print $ aggregate calculate input

parse :: String -> [String]
parse = map (foldr f "") . lines

f :: Char -> String -> String
f c cs 
  | "one"   `isPrefixOf` substr = c : '1' : cs
  | "two"   `isPrefixOf` substr = c : '2' : cs
  | "three" `isPrefixOf` substr = c : '3' : cs
  | "four"  `isPrefixOf` substr = c : '4' : cs
  | "five"  `isPrefixOf` substr = c : '5' : cs
  | "six"   `isPrefixOf` substr = c : '6' : cs
  | "seven" `isPrefixOf` substr = c : '7' : cs
  | "eight" `isPrefixOf` substr = c : '8' : cs
  | "nine"  `isPrefixOf` substr = c : '9' : cs
  | otherwise                   = c : cs
  where substr = c : cs
