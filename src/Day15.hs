module Day15 where 

import qualified Data.Map.Strict as M 
import Data.List (foldl')

-- key is the number, val is the iterations on which it occurred
type State = M.Map Int [Int]

input :: [Int]
input = [6,19,0,5,7,13,1] 

toState :: [Int] -> State
toState inp = M.fromList $ (\(n, i) -> (n, [i])) <$> zip inp [1..]

partOne :: State 
partOne = M.filter (\v -> head v == 2020) $ solve input 2020

partTwo :: State 
partTwo = M.filter (\v -> head v == 30000000) $ solve input 30000000

solve :: [Int] -> Int -> State 
solve inp limit = 
    fst $ foldl' 
        (\(state, prev) n ->
            case M.lookup prev state of 
                Just [] -> error "this should never happen"
                Just (_: []) -> -- prev value has only been spoken once before
                    insert n 0 state
                Just (last:nextLast:_) -> -- prev value has been spoken more than once
                    insert n (last - nextLast) state
                Nothing -> insert n 0 state
        ) (toState inp, 1) [(length inp + 1)..limit]
    where 
        insert turn n m = 
            (case M.lookup n m of 
                Just occurrences -> M.insert n (turn:occurrences) m
                Nothing -> M.insert n [turn] m, n) 
