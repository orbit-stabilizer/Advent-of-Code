module PartOne where

import Data.Text.Read
import qualified Data.Text as T

data Game = Game
  { gameId     :: Int
  , redCubes   :: Int
  , greenCubes :: Int
  , blueCubes  :: Int
  }

main :: IO ()
main = do
  input <- parse <$> readFile "z_input.txt"
  let games = map makeGame input
  print $ calculate games

parse :: String -> [T.Text]
parse = map (T.drop 5) . T.lines . (T.replace (T.pack ";") (T.pack ",")) . T.pack

makeGame :: T.Text -> Game
makeGame text =
  let
    gameId     = read . T.unpack $ T.takeWhile ((/=) ':') text :: Int
    redCubes   = getMaxColour "red" text
    greenCubes = getMaxColour "green" text
    blueCubes  = getMaxColour "blue" text
  in
    Game gameId redCubes greenCubes blueCubes
  where
    getMaxColour :: String -> T.Text -> Int
    getMaxColour colour =
      maximum . map (read . T.unpack . head . T.split ((==) ' ') . T.strip) . filter (T.isInfixOf (T.pack colour)) . T.split ((==) ',') . last . T.split ((==) ':')

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
