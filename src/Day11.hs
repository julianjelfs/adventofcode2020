module Day11 where

import Common (mapLines)
import Data.List ((!!))
import Data.Maybe (catMaybes)

dims :: (Int, Int)
dims = (97, 89)

type Grid = [String]

type Seat = Char

type SeatFilter = Grid -> (Int, Int) -> [Seat]

partOne :: IO Int
partOne = countOccupied . untilSettled 4 surroundingSeats <$> mapLines id "data/day11.txt"

partTwo :: IO Int
partTwo = countOccupied . untilSettled 5 visibleOccupiedSeats <$> mapLines id "data/day11.txt"

untilSettled :: Int -> SeatFilter -> Grid -> Grid
untilSettled n f g =
  let g' = iteration n f g
   in if g' == g
        then g
        else untilSettled n f g'

countOccupied :: Grid -> Int
countOccupied = length . filter (== '#') . unlines

iteration :: Int -> SeatFilter -> Grid -> Grid
iteration n f grid =
  ( \(y, r) ->
      ( \(x, c) ->
          if c == 'L'
            then
              if numOccupiedAdjacent f grid (x, y) == 0
                then '#'
                else c
            else
              if c == '#' && numOccupiedAdjacent f grid (x, y) >= n
                then 'L'
                else c
      )
        <$> xs r
  )
    <$> ys grid
  where
    ys = zip [0 .. snd dims]
    xs = zip [0 .. fst dims]

numOccupiedAdjacent :: SeatFilter -> Grid -> (Int, Int) -> Int
numOccupiedAdjacent f grid = length . filter (== '#') . f grid

surroundingSeats :: SeatFilter
surroundingSeats grid (x, y) =
  [ grid !! y' !! x'
    | x' <- [x - 1 .. x + 1],
      x' >= 0,
      x' <= fst dims,
      y' <- [y - 1 .. y + 1],
      (x', y') /= (x, y),
      y' >= 0,
      y' <= snd dims
  ]

visibleOccupiedSeats :: SeatFilter
visibleOccupiedSeats grid (x, y) = onlyOccupied $ catMaybes seats
  where
    onlyOccupied = filter (== '#')
    firstSeat [] = Nothing
    firstSeat (h : t) = if h == '#' || h == 'L' then Just h else firstSeat t
    seats = (\path -> firstSeat $ seatAt grid <$> path) <$> paths
    seatAt g (x, y) = g !! y !! x
    paths = [n, ne, e, se, s, sw, w, nw]
    n = (x,) <$> reverse [0 .. y - 1]
    ne = zip [x + 1 .. fst dims] (reverse [0 .. y - 1])
    e = (,y) <$> [x + 1 .. fst dims]
    se = zip [x + 1 .. fst dims] [y + 1 .. snd dims]
    s = (x,) <$> [y + 1 .. snd dims]
    sw = zip (reverse [0 .. x - 1]) [y + 1 .. snd dims]
    w = (,y) <$> reverse [0 .. x - 1]
    nw = zip (reverse [0 .. x - 1]) (reverse [0 .. y - 1])
