module Common where

import           Text.Parsec
import           Text.ParserCombinators.Parsec  ( ParseError
                                                , Parser
                                                )

mapLines :: (String -> a) -> FilePath -> IO [a]
mapLines f p = fmap f . lines <$> readFile p

readLines :: FilePath -> IO [String]
readLines p = lines <$> readFile p

numberParser :: Parsec String st Int
numberParser = rd <$> (plus <|> minus <|> number)
 where
  rd     = read :: String -> Int
  plus   = char '+' *> number
  minus  = (:) <$> char '-' <*> number
  number = many1 digit

parse :: Parser a -> String -> Either ParseError a
parse parser = Text.Parsec.parse parser []

safeHead :: [a] -> Maybe a
safeHead []      = Nothing
safeHead (x : _) = Just x

(!?) :: [a] -> Int -> Maybe a
(!?) = go 0
 where
  go _ [] _ = Nothing
  go n (h : t) i | n == i    = Just h
                 | otherwise = go (n + 1) t i
