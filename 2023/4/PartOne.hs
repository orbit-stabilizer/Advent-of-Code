{-# LANGUAGE OverloadedStrings #-}

module PartOne where

-- * Imports * --

import Data.Char (isDigit)

import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

-- * Operator Definitions * --

(|>) :: a -> (a -> b) -> b
(|>) = (&)

(!=) :: Eq a => a -> a -> Bool
(!=) = (/=)

-- * Code * --

main :: IO ()
main = do
  input <- parse <$> T.IO.readFile "sample.txt"

parse :: T.Text -> [T.Text]
parse text = 
  text
    |> T.replace ": " "-"
    |> T.lines
    |> fmap  (T.splitOn "-")
    |> fmap last

