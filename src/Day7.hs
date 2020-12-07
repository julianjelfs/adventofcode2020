module Day7
  ( partOne,
    partTwo,
  )
where

import Common (mapLines, numberParser, parse)
import Data.Either (rights)
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Parsec
import Text.ParserCombinators.Parsec
  ( Parser,
  )

type Rules = M.Map String [(String, Int)]

ruleParser :: Parser (String, [(String, Int)])
ruleParser = do
  colour <- manyTill anyChar (try (string " bags contain "))
  bags <- [] <$ string "no other bags" <|> sepBy bagParser (char ',' <* spaces)
  _ <- char '.'
  pure (colour, bags)

bagParser :: Parser (String, Int)
bagParser = do
  n <- numberParser <* spaces
  colour <- manyTill anyChar (try (string " bags") <|> try (string " bag"))
  pure (colour, n)

bagsThatCanContain :: String -> Rules -> S.Set String
bagsThatCanContain colour rules =
  foldr
    ( \p agg ->
        S.union agg (bagsThatCanContain p rules)
    )
    parents
    parents
  where
    parents = S.fromList . M.keys $ M.filter (any (\(c, _) -> c == colour)) rules

countContainedBags :: String -> Rules -> Int
countContainedBags colour rules =
  case M.lookup colour rules of
    Nothing -> error "could not find bag"
    Just bags' -> sum (subBag <$> bags')
      where
        subBag (c, n) = n + n * countContainedBags c rules

partOne :: IO Int
partOne =
  S.size . bagsThatCanContain "shiny gold" . M.fromList . rights
    <$> mapLines (Common.parse ruleParser) "data/day7.txt"

partTwo :: IO Int
partTwo =
  countContainedBags "shiny gold" . M.fromList . rights
    <$> mapLines (Common.parse ruleParser) "data/day7.txt"
