module DayTwo.CalculateCoordinates where

import Data.List.Split (splitOn)

data Direction = Forward | Up | Down


getCommands :: IO [String]
getCommands = do
  contents <- readFile "/Users/bilal.kilic/workspace/personal/advent-of-code/src/dayone/input.txt"
  return $ lines contents
  
getDirection :: String -> Direction
getDirection input
  | input == "forward" = Forward
  | input == "up" = Up
  | otherwise = Down

splitByEmptySpace :: String -> [String]
splitByEmptySpace input =
  splitOn input " "


parseInput :: String -> [(Direction), Int]
parseInput input =
  let splitted = splitByEmptySpace input

