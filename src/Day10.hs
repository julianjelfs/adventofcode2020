module Day10 where

import Common (mapLines)
import Data.List (foldl', sort)

partOne :: IO Int
partOne = countDiffs . sort <$> mapLines read "data/day10.txt"

partTwo :: IO Int
partTwo = countGroups . group . sort <$> mapLines read "data/day10.txt"

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

countGroups :: [[Int]] -> Int
countGroups =
  foldl'
    ( \total group ->
        case length group of
          3 -> total * 2
          4 -> total * 4
          5 -> total * 7
          _ -> total
    )
    1

group :: [Int] -> [[Int]]
group adapters = c : g
  where
    (g, c, _) =
      foldl'
        ( \(groups, currentGroup, prev) a ->
            if a - prev == 1
              then (groups, a : currentGroup, a)
              else (currentGroup : groups, [a], a)
        )
        ([], [0], 0)
        adapters
