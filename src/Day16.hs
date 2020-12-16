module Day16 where 

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List.Split (splitOn)
import Debug.Trace (traceShowId)

type FieldValues = M.Map String (S.Set Int)
type Ticket = S.Set Int
data Data = Data FieldValues Ticket [Ticket] deriving Show

numFields :: Int 
numFields = 20

partOne :: IO Int 
partOne = do 
    (Data fv _ nearby) <- parseData
    pure $ sumOfInvalidNearbyValues (allValidValues fv) (traceShowId nearby)
    where 
        allValidValues :: FieldValues -> S.Set Int
        allValidValues = 
            M.foldr (\s all -> S.union s all) S.empty

        sumOfInvalidNearbyValues :: S.Set Int -> [Ticket] -> Int
        sumOfInvalidNearbyValues valid = 
            foldr 
                (\ticket sumInvalid ->
                    sumInvalid + (sum $ S.difference ticket valid)
                ) 0

parseData :: IO Data
parseData = do
    allLines <- lines <$> readFile "data/day16.txt"
    pure $ Data (fieldVals allLines) (myTicket allLines) (nearby allLines)
    where 
        fieldValsStr = take numFields 
        fieldVals = parseFieldVals . fieldValsStr
        myTicket = parseTicket . head . take 1 . drop (numFields + 2) 
        nearby = fmap parseTicket . drop (numFields + 5) 

parseTicket :: String -> Ticket 
parseTicket str = S.fromList $ read <$> splitOn "," str

parseFieldVals :: [String] -> FieldValues
parseFieldVals = 
    foldr 
        (\str m -> let field = dropWhile (/= ':') str
                       r1 = takeWhile (/= ' ') $ drop 2 $ dropWhile (/= ':') str
                       r2 = drop 4 $ dropWhile (/= ' ') $ drop 2 $ dropWhile (/= ':') str
                   in M.insert field (S.union (splitRange r1) (splitRange r2)) m 
        ) M.empty
    where 
        splitRange r =
            let from = takeWhile (/= '-') r
                to = drop 1 $ dropWhile (/= '-') r
            in S.fromList [read from .. read to]

