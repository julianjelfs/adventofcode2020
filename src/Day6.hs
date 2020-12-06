module Day6
  ( partOne,
    partTwo,
  )
where

import qualified Data.Foldable as F
import Data.List.Split (linesBy)
import qualified Data.Set as S

anyAnswer :: [String] -> Int
anyAnswer = S.size . S.fromList . filter (/= ' ') . unwords

allAnswers :: [String] -> Int
allAnswers = S.size . intersections
  where
    intersections [] = S.empty
    intersections g = F.foldr1 S.intersection (S.fromList <$> g)

solve :: ([String] -> Int) -> IO Int
solve policy =
  sum
    . fmap policy
    . linesBy (== "")
    . lines
    <$> readFile "data/day6.txt"

partOne :: IO Int
partOne = solve anyAnswer

partTwo :: IO Int
partTwo = solve allAnswers
