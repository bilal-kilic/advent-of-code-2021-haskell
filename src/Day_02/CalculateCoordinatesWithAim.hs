module Day_02.CalculateCoordinatesWithAim where

import Day_02.CalculateCoordinates (Direction (..), parseCommands)

type CoordinateWithAim = (Int, Int, Int)

calculateDestinationCoordinates :: IO CoordinateWithAim
calculateDestinationCoordinates = do
  commands <- parseCommands
  return (foldl calculateCoordinate (0, 0, 0) commands)

calculateCoordinate :: CoordinateWithAim -> (Direction, Int) -> CoordinateWithAim
calculateCoordinate currentCoordinate command
  | direction == Forward = (x + distance, y + (aim * distance), aim)
  | direction == Up = (x, y, aim - distance)
  | direction == Down = (x, y, aim + distance)
  where
    direction = fst command
    distance = snd command
    (x, y, aim) = currentCoordinate

printDayTwoResultWithAim :: IO ()
printDayTwoResultWithAim = do
  finalCoordinate <- calculateDestinationCoordinates
  let (x, y, _) = finalCoordinate
  print (x * y)
