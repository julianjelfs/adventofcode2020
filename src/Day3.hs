module Day3
  ( partOne,
    partTwo,
  )
where

import Common (mapLines)
import Data.List (cycle, product)

partOne :: IO Int
partOne = do
  grid <- mapLines cycle "data/day3.txt"
  pure $ countTreesForSlope grid (\(x, y) -> (x + 3, y + 1))

partTwo :: IO Int
partTwo = do
  grid <- mapLines cycle "data/day3.txt"
  pure $
    product $ countTreesForSlope grid <$> slopes
  where
    slopeOne (x, y) = (x + 1, y + 1)
    slopeTwo (x, y) = (x + 3, y + 1)
    slopeThree (x, y) = (x + 5, y + 1)
    slopeFour (x, y) = (x + 7, y + 1)
    slopeFive (x, y) = (x + 1, y + 2)
    slopes = [slopeOne, slopeTwo, slopeThree, slopeFour, slopeFive]

countTreesForSlope :: [String] -> ((Int, Int) -> (Int, Int)) -> Int
countTreesForSlope grid slope = go (0, 0) 0 grid
  where
    go coord@(x, y) trees g
      | y >= length g = trees
      | otherwise =
        case g !! y !! x of
          '#' -> go (slope coord) (trees + 1) g
          _ -> go (slope coord) trees g
