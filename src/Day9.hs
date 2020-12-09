module Day9 where

import Common (mapLines)
import Data.List (inits, nub, tails)
import Data.Maybe (mapMaybe)

findFirstInvalid :: [Int] -> Maybe Int
findFirstInvalid nums = go 0
  where
    go cursor =
      let slice = take 25 $ drop cursor nums
          combos = [i + j | i <- slice, j <- slice, i /= j]
       in case drop (cursor + 25) nums of
            [] -> Nothing
            (h : _) -> if h `elem` combos then go (cursor + 1) else Just h

partOne :: IO (Maybe Int)
partOne = do
  nums <- mapLines read "data/day9.txt"
  pure $ findFirstInvalid nums

partTwo :: IO [(Int, [Int])]
partTwo = do
  nums <- mapLines read "data/day9.txt"
  pure $
    mapMaybe
      ( \range ->
          if sum range == 400480901
            then Just (minimum range + maximum range, range)
            else Nothing
      )
      (contiguousSublists nums)
  where
    contiguousSublists :: [Int] -> [[Int]]
    contiguousSublists nums = filter (\r -> length r >= 2) $ nub $ concatMap tails (inits nums)
