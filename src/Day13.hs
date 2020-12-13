module Day13 where

import Data.Foldable (minimumBy)
import Math.NumberTheory.Moduli.Chinese (chineseRemainder)

earliest :: Int
earliest = 1008832

input :: [String]
input =
  ["23", "x", "x", "x", "x", "x", "x", "x", "x", "x", "x", "x", "x", "41", "x", "x", "x", "x", "x", "x", "x", "x", "x", "449", "x", "x", "x", "x", "x", "x", "x", "x", "x", "x", "x", "x", "x", "x", "x", "x", "x", "13", "19", "x", "x", "x", "x", "x", "x", "x", "x", "x", "29", "x", "991", "x", "x", "x", "x", "x", "37", "x", "x", "x", "x", "x", "x", "x", "x", "x", "x", "17"]

partOne :: Int
partOne = (time - earliest) * busId
  where
    (busId, time) = minimumBy lowestTime $ earliestBus earliest <$> buses
    lowestTime a b = compare (snd a) (snd b)
    buses = read <$> filter (/= "x") input

partTwo :: (Maybe Integer)
partTwo = chineseRemainder buses
  where
    buses = toInt <$> filter noxs (zip [0 ..] input)
    noxs (_, busId) = busId /= "x"
    toInt (offset, busId) = (- offset, read busId)

earliestBus :: Int -> Int -> (Int, Int)
earliestBus limit busId = go [0, busId ..]
  where
    go [] = (0, 0)
    go (h : t)
      | h >= limit = (busId, h)
      | otherwise = go t
