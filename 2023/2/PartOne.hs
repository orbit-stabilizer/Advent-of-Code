{-# LANGUAGE OverloadedStrings #-}

module PartOne where

import Data.Text.Read
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
 
data Game = Game
  { gameId     :: Int
  , redCubes   :: Int
  , greenCubes :: Int
  , blueCubes  :: Int
  }

main :: IO ()
main = do
  input <- parse <$> T.IO.readFile "z_input.txt"
  let games = map makeGame input
  print $ calculate games

parse :: T.Text -> [T.Text]
parse = map (T.drop 5) . T.lines . T.replace ";" ","

makeGame :: T.Text -> Game
makeGame text =
  let
    pText      = T.split (== ',') . last $ T.split (== ':') text
    gameId     = read . T.unpack $ T.takeWhile (/= ':') text
    redCubes   = getMaxColour "red" pText
    greenCubes = getMaxColour "green" pText
    blueCubes  = getMaxColour "blue" pText
    getMaxColour :: T.Text -> [T.Text] -> Int
    getMaxColour colour pText =
      maximum
       . map (read . T.unpack . head . T.words)
       $ filter (T.isInfixOf colour) pText
  in
    Game gameId redCubes greenCubes blueCubes

calculate :: [Game] -> Int
calculate = sum . map gameId . filter isValidGame
  where
    isValidGame :: Game -> Bool
    isValidGame (Game {redCubes = r, greenCubes = g, blueCubes = b}) = 
      let 
        maxRedCubes = 12
        maxGreenCubes = 13
        maxBlueCubes = 14
      in
        r <= maxRedCubes && g <= maxGreenCubes && b <= maxBlueCubes
