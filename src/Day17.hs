module Day17 where

import Common (mapLines)
import Data.List (foldl')
import qualified Data.Map.Strict as M

type Cube = (Int, Int, Int)

data CubeState = Active | Inactive deriving (Eq)

-- represents all of the active points in the space
type Space = M.Map Cube CubeState

partOne :: IO Int
partOne = do
  space <- toSpace <$> mapLines id "data/day17.txt"
  pure $ countActive $ performSix space
  where
    countActive =
      M.size . M.filter (== Active)
    performSix =
      foldr (.) id (replicate 6 (iteration . padNeighbours))

iteration :: Space -> Space
iteration space =
  M.foldrWithKey
    ( \k v m ->
        case v of
          Active ->
            case length $ activeNeighbouringPoints space k of
              2 -> M.insert k v m
              3 -> M.insert k v m
              _ -> M.insert k Inactive m
          Inactive ->
            case length $ activeNeighbouringPoints space k of
              3 -> M.insert k Active m
              _ -> M.insert k v m
    )
    M.empty
    space

activeNeighbouringPoints :: Space -> Cube -> [Cube]
activeNeighbouringPoints space cube =
  filter
    ( \c ->
        case M.lookup c space of
          Nothing -> False
          Just s -> s == Active
    )
    (neighbouringPoints cube)

neighbouringPoints :: Cube -> [Cube]
neighbouringPoints (px, py, pz) =
  [ (x, y, z)
    | x <- [px -1 .. px + 1],
      y <- [py -1 .. py + 1],
      z <- [pz -1 .. pz + 1],
      (x, y, z) /= (px, py, pz)
  ]

-- add neighbours for all cubes if necessary
padNeighbours :: Space -> Space
padNeighbours space =
  M.foldrWithKey
    ( \k _ m ->
        foldr
          ( \p m ->
              case M.lookup p m of
                Nothing ->
                  M.insert p Inactive m
                Just _ -> m
          )
          m
          (neighbouringPoints k)
    )
    space
    space

-- PARSING --
toSpace :: [String] -> Space
toSpace rows =
  foldl'
    ( \m (y, row) ->
        foldl'
          ( \m (x, col) ->
              M.insert (x, y, 0) (cubeState col) m
          )
          m
          (zip [0 ..] row)
    )
    M.empty
    (zip [0 ..] rows)
  where
    cubeState '#' = Active
    cubeState '.' = Inactive
    cubeState _ = error "invalid cube state"
