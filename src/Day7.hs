module Day7
  ( partOne,
    partTwo,
    parseRule,
  )
where

import Common (mapLines, numberParser, parse)
import Text.Parsec
import Text.ParserCombinators.Parsec
  ( Parser,
  )

data NumberOfBags = NumberOfBags String Int deriving (Show)

ruleParser :: Parser (String, [NumberOfBags])
ruleParser = do
  colour <- manyTill anyChar (try (string " bags contain "))
  bags <- [] <$ string "no other bags" <|> sepBy bagParser (char ',' <* spaces)
  _ <- char '.'
  pure $ (colour, bags)

bagParser :: Parser NumberOfBags
bagParser = do
  n <- numberParser <* spaces
  colour <- manyTill anyChar (try (string " bags") <|> try (string " bag"))
  pure $ NumberOfBags colour n

parseRule = Common.parse ruleParser

partOne :: IO [Either ParseError (String, [NumberOfBags])]
partOne = mapLines parseRule "data/day7.txt"

partTwo :: IO Int
partTwo = undefined
