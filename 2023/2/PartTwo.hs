{-# LANGUAGE OverloadedStrings #-}

module PartTwo where

-- * Imports * --

import Data.Char (isDigit)
import Data.Function ((&))

import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import qualified Data.Text.Read as T.R 

import PartOne (parse, makeGame, Game (..))


main :: IO ()
main = do
  input <- parse <$> T.IO.readFile "z_input.txt"
  let games = map makeGame input
  print $ calculate games

calculate :: [Game] -> Int
calculate = sum . map power
  where
    power :: Game -> Int
    power (Game {redCubes = r, greenCubes = g, blueCubes = b}) = r * g * b
