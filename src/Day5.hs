module Day5
  ( partOne,
    partTwo,
  )
where

import Common (mapLines)
import Data.Char (digitToInt)
import Data.Foldable (foldl')
import qualified Data.Set as S

partOne :: IO Int
partOne = maximum <$> mapLines parseBoardingPass "data/day5.txt"

partTwo :: IO [Int]
partTwo = myBoardingPass <$> mapLines parseBoardingPass "data/day5.txt"

myBoardingPass :: [Int] -> [Int]
myBoardingPass passes =
  filter (\p -> p > minPass && p < maxPass) (missingPasses passes)
  where
    minPass = minimum passes
    maxPass = maximum passes

missingPasses :: [Int] -> [Int]
missingPasses passes = S.toList $ S.difference all taken
  where
    all = S.fromList [0 .. 1023]
    taken = S.fromList passes

parseBoardingPass :: String -> Int
parseBoardingPass =
  binaryStringToInt . toBinaryString

toBinaryString :: String -> String
toBinaryString = fmap charToBit
  where
    charToBit = \case
      'F' -> '0'
      'B' -> '1'
      'L' -> '0'
      'R' -> '1'
      _ -> error "dodgy input"

binaryStringToInt :: String -> Int
binaryStringToInt = foldl' (\agg c -> agg * 2 + (digitToInt c)) 0
