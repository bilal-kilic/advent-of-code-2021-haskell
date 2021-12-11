module DayTwo.CalculateCoordinates
  ( Direction (..),
    parseCommands,
    printDayTwoResult,
  )
where

import DayOne.MeasureDepth (toInt)

data Direction = Forward | Up | Down deriving (Eq)

type Coordinate = (Int, Int)

getCommands :: IO [String]
getCommands = do
  contents <- readFile "/Users/bilal.kilic/workspace/personal/advent-of-code/src/daytwo/input.txt"
  return $ lines contents

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

getDirection :: String -> Direction
getDirection input
  | input == "forward" = Forward
  | input == "up" = Up
  | otherwise = Down

parseCommands :: IO [(Direction, Int)]
parseCommands = do
  inputLines <- getCommands
  return (map parseCommand inputLines)

parseCommand :: String -> (Direction, Int)
parseCommand input =
  let splittedWords = wordsWhen (== ' ') input
      firstElement = head splittedWords
      secondElement = splittedWords !! 1
   in (getDirection firstElement, toInt secondElement)

calculateDestinationCoordinates :: IO Coordinate
calculateDestinationCoordinates = do
  commands <- parseCommands
  return (foldl calculateCoordinate (0, 0) commands)

calculateCoordinate :: Coordinate -> (Direction, Int) -> Coordinate
calculateCoordinate currentCoordinate command
  | direction == Forward = (x + distance, y)
  | direction == Up = (x, y - distance)
  | direction == Down = (x, y + distance)
  where
    direction = fst command
    distance = snd command
    x = fst currentCoordinate
    y = snd currentCoordinate

printDayTwoResult :: IO ()
printDayTwoResult = do
  finalCoordinate <- calculateDestinationCoordinates
  print (uncurry (*) finalCoordinate)
