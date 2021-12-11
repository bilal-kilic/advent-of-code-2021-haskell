module Day_03.GetPowerConsumption where

import Data.Bifunctor (second)
import Data.Char (digitToInt)

getDiagnostics :: IO [String]
getDiagnostics = do
  contents <- readFile "/Users/bilal.kilic/workspace/personal/advent-of-code/src/day_03/input.txt"
  let stringLines = lines contents
  return stringLines

getMostCommonBitInIndex :: [Int] -> Int
getMostCommonBitInIndex diagnosticsRow =
  if sum diagnosticsRow > div (length diagnosticsRow) 2
    then 1
    else 0

getDiagnosticRowAtIndex :: [String] -> Int -> (Int, [Int])
getDiagnosticRowAtIndex diagnostics index =
  let currentBase = length (head diagnostics) - index
      elementsAtIndex = map (!! index) diagnostics
      row = map digitToInt elementsAtIndex
   in (currentBase, row)

getMostCommonBitsInRows :: IO [(Int, Int)]
getMostCommonBitsInRows = do
  diagnostics <- getDiagnostics
  let numberOfDigits = length (head diagnostics) - 1
  let rows = map (getDiagnosticRowAtIndex diagnostics) [0 .. numberOfDigits]
  return (map (Data.Bifunctor.second getMostCommonBitInIndex) rows)

toBaseTen :: (Int, Int) -> Int
toBaseTen bit =
  let (base, digit) = bit
   in digit * (2 ^ (base - 1))

calculatePowerConsumption :: IO Int
calculatePowerConsumption = do
  mostCommonBitsInRow <- getMostCommonBitsInRows
  let gammaRate = foldl (\total bit -> toBaseTen bit + total) 0 mostCommonBitsInRow
  let numberOfDigits = length mostCommonBitsInRow
  let maxBinaryNumberWithGivenDigits = foldl(\total n -> toBaseTen (n, 1) + total) 0 [1.. numberOfDigits]
  let epsilonRate = maxBinaryNumberWithGivenDigits - gammaRate
    
  return (gammaRate * epsilonRate)

printPowerConsumption :: IO ()
printPowerConsumption = do
  result <- calculatePowerConsumption
  print result
