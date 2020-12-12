module Day12 where

import Common (mapLines)
import Data.List (foldl')

data State = State (Int, Int) Int

data Instr
  = GoNorth Int
  | GoSouth Int
  | GoEast Int
  | GoWest Int
  | TurnLeft Int
  | TurnRight Int
  | GoForwards Int
  deriving (Show)

partOne :: IO Int
partOne = measure . follow <$> mapLines parseInstruction "data/day12.txt"

partTwo :: IO Int
partTwo = undefined

steps :: Int -> Int
steps 90 = 1
steps 180 = 2
steps 270 = 3
steps _ = error "not again"

follow :: [Instr] -> State
follow =
  foldl'
    ( \(State (x, y) d) i ->
        case i of
          GoNorth n -> State (x, y + n) d
          GoSouth n -> State (x, y - n) d
          GoEast n -> State (x + n, y) d
          GoWest n -> State (x - n, y) d
          TurnLeft n -> State (x, y) (turn (-) n d)
          TurnRight n -> State (x, y) (turn (+) n d)
          GoForwards n -> State (forwards n (x, y) d) d
    )
    (State (0, 0) 0)
  where
    forwards n (x, y) 0 = (x + n, y)
    forwards n (x, y) 1 = (x, y - n)
    forwards n (x, y) 2 = (x - n, y)
    forwards n (x, y) 3 = (x, y + n)
    forwards _ _ _ = error "oh no"
    turn op n d = (d `op` steps n) `mod` 4

measure :: State -> Int
measure (State (x, y) _) = abs x + abs y

parseInstruction :: String -> Instr
parseInstruction = \case
  ('N' : n) -> GoNorth (read n)
  ('S' : n) -> GoSouth (read n)
  ('E' : n) -> GoEast (read n)
  ('W' : n) -> GoWest (read n)
  ('L' : n) -> TurnLeft (read n)
  ('R' : n) -> TurnRight (read n)
  ('F' : n) -> GoForwards (read n)
  _ -> error "invalid instruction"
