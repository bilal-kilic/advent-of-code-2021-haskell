module DayOne.MeasureDepth
  ( getLines,
  )
where

getLines :: IO [Int]
getLines = do
  contents <- readFile "/Users/bilal.kilic/workspace/personal/advent-of-code/src/dayone/input.txt"
  return (map (\n -> read n :: Int) (lines contents))

getPreviousElement :: [Int] -> Int -> Int
getPreviousElement list element =
  list !! ((list `elemIndex` element) - 1) 

measureDepth = do
  numbers <- getLines
  return
    ( foldl
        ( \total number ->            
            if number > (getPreviousElement numbers number)
              then number
              else total
        )
        0
        numbers
    )

