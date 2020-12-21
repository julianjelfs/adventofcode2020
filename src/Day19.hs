module Day19 where

import Common (parse)
import qualified Data.Map as M
-- import Data.Maybe (catMaybes)
import Text.Parsec
import Text.ParserCombinators.Parsec
  ( Parser,
  )

type Rules = M.Map String String

messageParser :: String -> Rules -> Parser String
messageParser num rules = case M.lookup num rules of
  Nothing -> error "couldn't find the rule"
  Just "\"a\"" -> string "a"
  Just "\"b\"" -> string "b"
  Just rule ->
    let w = words rule
        p = flip messageParser rules
     in case length w of
          1 -> p (w !! 0)
          2 -> p (w !! 0) >> p (w !! 1)
          3 -> p (w !! 0) <|> p (w !! 2)
          5 -> (p (w !! 0) >> p (w !! 1)) <|> (p (w !! 3) >> p (w !! 4))
          _ -> error ("unexpected pattern:" <> rule)

getMessages :: IO [String]
getMessages = drop 140 <$> allLines

getRules :: IO Rules
getRules = M.fromList . fmap tuplify . take 139 <$> allLines
  where
    tuplify s =
      (takeWhile (/= ':') s, drop 2 $ dropWhile (/= ':') s)

allLines :: IO [String]
allLines = lines <$> readFile "data/day19.txt"

partOne :: IO [Either ParseError String]
partOne = do
  p <- messageParser "0" <$> getRules
  m <- getMessages
  pure $ Common.parse p <$> m
