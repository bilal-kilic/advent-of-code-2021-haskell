module DayOne.MeasureDepth
  ( getMeasurements,
    printResult,
  )
where

toInt :: String -> Int
toInt string = read string :: Int

getMeasurements :: IO [Int]
getMeasurements = do
  contents <- readFile "/Users/bilal.kilic/workspace/personal/advent-of-code/src/dayone/input.txt"
  let stringLines = lines contents
  let intLines = map toInt stringLines
  return intLines

countDeeper :: [Int] -> (Int, Int) -> Int -> Int
countDeeper numbers currentIndexed total
  | prevIndex < 0 = total
  | current > prev = total + 1
  | otherwise = total
  where
    prevIndex = fst currentIndexed -1
    prev = numbers !! prevIndex
    current = snd currentIndexed

measureDepth :: IO Int
measureDepth = do
  measurements <- getMeasurements
  return (foldl (flip (countDeeper measurements)) 0 (zip [0 ..] measurements))

printResult :: IO ()
printResult = do
  result <- measureDepth
  print result
