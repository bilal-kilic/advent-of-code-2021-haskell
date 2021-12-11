module DayOne.MeasureSlidingDepth where

import DayOne.MeasureDepth

measureSlidingDepth :: IO Int
measureSlidingDepth = do
  measurements <- getMeasurements
  return (foldl (flip (countSlidingDepthChange measurements)) 0 (zip [0 ..] measurements))

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