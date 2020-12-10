module Day10 where

import Common (mapLines)
import Data.List (foldl', sort)

partOne :: IO Int
partOne = countDiffs . sort <$> mapLines read "data/day10.txt"

countDiffs :: [Int] -> Int
countDiffs =
  (\(ones, threes, _) -> ones * (threes + 1))
    . foldl'
      ( \(ones, threes, prev) a ->
          case a - prev of
            1 -> (ones + 1, threes, a)
            3 -> (ones, threes + 1, a)
            _ -> (ones, threes, a)
      )
      (0, 0, 0)

partTwo :: IO [(Int, [Int])]
partTwo = undefined
