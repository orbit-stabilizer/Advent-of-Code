module PartOne where

import Data.Char (isDigit)

main :: IO ()
main = do
  input <- lines <$> readFile "document.txt"
  print $ aggregate calculate input

calculate :: String -> Int
calculate = (\x -> read [head x, last x]) . filter isDigit

aggregate :: Num b => (a -> b) -> [a] -> b
aggregate f = sum . map f
