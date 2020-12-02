module Day2
  ( partOne,
    partTwo,
    Entry (..),
    policyTwo,
  )
where

import Common (mapLines, numberParser, parse)
import Data.List ((!!))
import Data.Maybe (catMaybes)
import Text.Parsec
import Text.ParserCombinators.Parsec
  ( Parser,
  )

data Entry = Entry Int Int Char String

entryParser :: Parser Entry
entryParser = do
  min <- numberParser
  _ <- char '-'
  max <- numberParser
  _ <- char ' '
  ch <- anyChar
  _ <- string ": "
  pw <- many anyChar
  pure $ Entry min max ch pw

parseEntry :: String -> Maybe Entry
parseEntry str = case Common.parse entryParser str of
  Left _ -> Nothing
  Right e -> Just e

policyOne :: Entry -> Bool
policyOne (Entry min max char pw) =
  count >= min && count <= max
  where
    count = length $ filter (== char) pw

policyTwo :: Entry -> Bool
policyTwo (Entry min max char pw) =
  let mn = pw !! (min - 1)
      mx = pw !! (max - 1)
   in (mn == char || mx == char)
        && not
          (mn == char && mx == char)

partOne :: IO Int
partOne = countValidPasswords policyOne

partTwo :: IO Int
partTwo = countValidPasswords policyTwo

countValidPasswords :: (Entry -> Bool) -> IO Int
countValidPasswords policy =
  length . filter policy . catMaybes <$> mapLines parseEntry "data/day2.txt"
