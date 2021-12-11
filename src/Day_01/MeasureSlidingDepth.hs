module Day_01.MeasureSlidingDepth where

import Day_01.MeasureDepth

measureSlidingDepth :: IO Int
measureSlidingDepth = do
  measurements <- getMeasurements
  let indexedMeasurements = zip [0 ..] measurements
  return (foldl (flip (countSlidingDepthChange measurements)) 0 indexedMeasurements)

countSlidingDepthChange :: [Int] -> (Int, Int) -> Int -> Int
countSlidingDepthChange measurements currentIndexed totalIncrease
  | currentIndex + 3 >= length measurements = totalIncrease
  | nextWindow > currentWindow = totalIncrease + 1
  | otherwise = totalIncrease
  where
    currentIndex = fst currentIndexed
    currentWindow = measurements !! currentIndex
    nextWindow = measurements !! (currentIndex + 3)

printSlidingDepthResult :: IO ()
printSlidingDepthResult = do
  result <- measureSlidingDepth
  print result
