module Day19 where

import qualified Data.Map as M

type Rules = M.Map String String

data Rule
  = Letter Char
  | And Rule Rule
  | Or Rule Rule
  deriving (Show)

buildAst :: String -> Rules -> Rule
buildAst num rules = case M.lookup num rules of
  Nothing -> error "couldn't find the rule"
  Just "\"a\"" -> Letter 'a'
  Just "\"b\"" -> Letter 'b'
  Just rule ->
    let w = words rule
        ast = flip buildAst rules
     in case length w of
          1 -> ast (w !! 0)
          2 -> And (ast (w !! 0)) (ast (w !! 1))
          3 -> Or (ast (w !! 0)) (ast (w !! 2))
          5 -> Or (And (ast (w !! 0)) (ast (w !! 1))) (And (ast (w !! 3)) (ast (w !! 4)))
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

partOne :: IO Rule
partOne = buildAst "0" <$> getRules
