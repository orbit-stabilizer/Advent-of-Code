{-# LANGUAGE OverloadedStrings #-}

module PartOne where

-- * Imports * --

import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

-- * Code * --

main :: IO ()
main = do
  input <- T.lines <$> T.IO.readFile "z_input.txt"
 print input

-- find all things to left and right, and then transpose
