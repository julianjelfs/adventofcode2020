module Day12 where

import Common (mapLines)
import Data.List (foldl')

type Pos = (Int, Int)

type State = (Pos, Int)

type State2 = (Pos, Pos)

type Instr = (Char, Int)

partOne :: IO Int
partOne = manhattanDistance . followOne <$> mapLines parseInstruction "data/day12.txt"

partTwo :: IO Int
partTwo = manhattanDistance . followTwo <$> mapLines parseInstruction "data/day12.txt"

rotate90 :: Pos -> Pos
rotate90 (x, y) = (y, - x)

steps :: Int -> Int
steps 90 = 1
steps 180 = 2
steps 270 = 3
steps (-90) = 3
steps (-180) = 2
steps (-270) = 1
steps _ = error "not again"

followTwo :: [Instr] -> State2
followTwo =
  foldl'
    ( \(ship, waypoint) i ->
        case i of
          ('N', n) -> (ship, moveY n waypoint)
          ('S', n) -> (ship, moveY (- n) waypoint)
          ('E', n) -> (ship, moveX n waypoint)
          ('W', n) -> (ship, moveX (- n) waypoint)
          ('L', n) -> (ship, rotate (- n) waypoint)
          ('R', n) -> (ship, rotate n waypoint)
          ('F', n) -> (forwards n ship waypoint, waypoint)
          _ -> (ship, waypoint)
    )
    ((0, 0), (10, 1))
  where
    forwards n (sx, sy) (wx, wy) = (sx + n * wx, sy + n * wy)
    moveY n (x, y) = (x, y + n)
    moveX n (x, y) = (x + n, y)
    rotate n (x, y) = rotateN (x, y)
      where
        rotateN = foldr (.) id (replicate (steps n) rotate90)

followOne :: [Instr] -> State
followOne =
  foldl'
    ( \((x, y), d) i ->
        case i of
          ('N', n) -> ((x, y + n), d)
          ('S', n) -> ((x, y - n), d)
          ('E', n) -> ((x + n, y), d)
          ('W', n) -> ((x - n, y), d)
          ('L', n) -> ((x, y), turn (-) n d)
          ('R', n) -> ((x, y), turn (+) n d)
          ('F', n) -> (forwards n (x, y) d, d)
          _ -> ((x, y), d)
    )
    ((0, 0), 0)
  where
    forwards n (x, y) 0 = (x + n, y)
    forwards n (x, y) 1 = (x, y - n)
    forwards n (x, y) 2 = (x - n, y)
    forwards n (x, y) 3 = (x, y + n)
    forwards _ _ _ = error "oh no"
    turn op n d = (d `op` steps n) `mod` 4

manhattanDistance :: (Pos, a) -> Int
manhattanDistance ((x, y), _) = abs x + abs y

parseInstruction :: String -> Instr
parseInstruction = \case
  (c : n) -> (c, read n)
  _ -> error "invalid instruction"
