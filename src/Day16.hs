module Day16 where 

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List.Split (splitOn)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)

type FieldValues = M.Map String (S.Set Int)
type Ticket = [Int]
data Data = Data FieldValues Ticket [Ticket] deriving Show
type ValidFields = M.Map Int (S.Set String)

numFields :: Int 
numFields = 20

partOne :: IO Int 
partOne = do 
    (Data fv _ nearby) <- parseData
    pure $ sumOfInvalidNearbyValues (allValidValues fv) nearby
    where 
        sumOfInvalidNearbyValues :: S.Set Int -> [Ticket] -> Int
        sumOfInvalidNearbyValues valid = 
            foldr 
                (\ticket sumInvalid ->
                    sumInvalid + (sum $ S.difference (S.fromList ticket) valid)
                ) 0

partTwo :: IO Int 
partTwo = do 
    (Data fv myTicket nearby) <- parseData
    let allValid = allValidValues fv
        validTickets = filter (\t -> S.empty == S.difference (S.fromList t) allValid) (myTicket : nearby)
    pure 
        $ product
        $ fmap (\idx -> myTicket !! idx)
        $ fmap (head . snd)
        $ filter (\(field, _) -> "departure" `isPrefixOf` field) 
        $ collapseIndices 
        $ possibleIndexesForFields fv validTickets

possibleIndexesForFields :: FieldValues -> [Ticket] -> [(String, [Int])]
possibleIndexesForFields fields tickets =
    (\(fieldName, range) -> (fieldName, possibleIndexesForRange range tickets)) <$> M.assocs fields 

possibleIndexesForRange :: S.Set Int -> [Ticket] -> [Int]
possibleIndexesForRange range tickets = 
    catMaybes $ 
    fmap 
        (\idx -> let values = S.fromList $ (\t -> t !! idx) <$> tickets
                 in if S.difference values range == S.empty then 
                     Just idx else Nothing
        ) [0..19] 

collapseIndices :: [(String, [Int])] -> [(String, [Int])] 
collapseIndices fields 
    | settled = fields 
    | otherwise =  
        collapseIndices $ 
        fmap (\(field, indices) -> (field, filter (\i -> i `notElem` resolvedIndices) indices) ) unresolved 
            <> resolved
    where 
        settled = length resolved == length fields
        resolved = filter (\(_, indices) -> length indices == 1) fields
        unresolved = filter (\(_, indices) -> length indices > 1) fields
        resolvedIndices = concatMap (\(_, indices) -> indices) resolved

allValidValues :: FieldValues -> S.Set Int
allValidValues = 
    M.foldr (\s all -> S.union s all) S.empty

-- HIDEOUS PARSING
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
parseTicket str = read <$> splitOn "," str

parseFieldVals :: [String] -> FieldValues
parseFieldVals = 
    foldr 
        (\str m -> let field = takeWhile (/= ':') str
                       r1 = takeWhile (/= ' ') $ drop 2 $ dropWhile (/= ':') str
                       r2 = drop 4 $ dropWhile (/= ' ') $ drop 2 $ dropWhile (/= ':') str
                   in M.insert field (S.union (splitRange r1) (splitRange r2)) m 
        ) M.empty
    where 
        splitRange r =
            let from = takeWhile (/= '-') r
                to = drop 1 $ dropWhile (/= '-') r
            in S.fromList [read from .. read to]

