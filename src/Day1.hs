module Day1
  ( partOne,
    partTwo,
  )
where

import Common (mapLines)

partOne :: IO Int
partOne = do
  numbers <- mapLines read "data/day1.txt"
  let pairs =
        [ (i, j) | i <- numbers, j <- numbers, i + j == 2020
        ]
  pure $ case pairs of
    (i, j) : _ -> i * j
    _ -> 0

partTwo :: IO Int
partTwo = do
  numbers <- mapLines read "data/day1.txt"
  let pairs =
        [ (i, j, k) | i <- numbers, j <- numbers, k <- numbers, i + j + k == 2020
        ]
  pure $ case pairs of
    (i, j, k) : _ -> i * j * k
    _ -> 0
