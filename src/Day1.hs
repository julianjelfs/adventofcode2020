module Day1
  ( solve
  )
where

import           Common                         ( mapLines )
import           Text.Read                      ( readMaybe )

solve :: IO (Maybe Int)
solve = do
  l <- mapLines (fmap fuel . readMaybe) "data/day1.txt"
  pure $ fmap sum . sequence $ l

fuel :: Int -> Int
fuel n | f > 0     = f + fuel f
       | otherwise = 0
  where f = (n `div` 3) - 2

