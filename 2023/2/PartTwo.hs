{-# LANGUAGE OverloadedStrings #-}

module PartTwo where

-- * Imports * --

import Data.Char (isDigit)
import Data.Function ((&))

import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import qualified Data.Text.Read as T.R 

-- * Globals * -- 

maxRedCubes = 12
maxGreenCubes = 13
maxBlueCubes = 14

-- * Data Definitions * --

data Game = Game
  { gameId     :: Int
  , redCubes   :: Int
  , greenCubes :: Int
  , blueCubes  :: Int
  }

-- * Operator Definitions * --

(|>) :: a -> (a -> b) -> b
(|>) = (&)

(!=) :: Eq a => a -> a -> Bool
(!=) = (/=)

-- * Code * --

main :: IO ()
main = do
  input <- parse <$> T.IO.readFile "z_input.txt"
  let games = map makeGame input
  print $ calculate games

parse :: T.Text -> [T.Text]
parse text =
  text
    |> T.filter (!= ' ')
    |> T.replace ";" ","
    |> T.lines
    |> map (T.dropWhile (not . isDigit))

makeGame :: T.Text -> Game
makeGame text = Game gameId redCubes greenCubes blueCubes
  where
    gameId     = either (error "gameId parse error") fst . T.R.decimal $ text
    redCubes   = getMaxColour "red"  
    greenCubes = getMaxColour "green"
    blueCubes  = getMaxColour "blue"

    pText      = T.split (== ',') . T.takeWhileEnd (!= ':') $ text

    getMaxColour :: T.Text -> Int
    getMaxColour colour =
      pText
        |> filter (T.isInfixOf colour)
        |> map  (either (error "colour parse error") fst . T.R.decimal)
        |> maximum
    
calculate :: [Game] -> Int
calculate = sum . map power
  where
    power :: Game -> Int
    power (Game {redCubes = r, greenCubes = g, blueCubes = b}) = r * g * b
