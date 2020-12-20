module Day18 where

import Common (hush, mapLines, numberParser, parse)
import Text.Parsec
import Text.ParserCombinators.Parsec
  ( Parser,
  )

data Expr
  = Nat Int
  | Add Expr Expr
  | Mult Expr Expr

evaluate :: Expr -> Int
evaluate (Nat n) = n
evaluate (Add e1 e2) = evaluate e1 + evaluate e2
evaluate (Mult e1 e2) = evaluate e1 * evaluate e2

addParser :: Parser (Expr -> Expr -> Expr)
addParser = Add <$ char '+'

multParser :: Parser (Expr -> Expr -> Expr)
multParser = Mult <$ char '*'

valParser :: Parser Expr
valParser =
  Nat <$> numberParser

bracketed :: Parser Expr -> Parser Expr
bracketed = between (char '(') (char ')')

chunkParser :: Parser Expr -> Parser Expr
chunkParser expP =
  valParser <|> bracketed expP

-- straight left to right
expressionParser1 :: Parser Expr
expressionParser1 = chunkParser expressionParser1 `chainl1` (addParser <|> multParser)

-- add first and then multiply
expressionParser2 :: Parser Expr
expressionParser2 = (chunkParser expressionParser2 `chainl1` addParser) `chainl1` multParser

partOne :: IO Int
partOne = solve expressionParser1

partTwo :: IO Int
partTwo = solve expressionParser2

solve :: Parser Expr -> IO Int
solve p = sum <$> mapLines (parseLine p) "data/day18.txt"
  where
    parseLine p = maybe 0 evaluate . hush . Common.parse p . filter (/= ' ')
