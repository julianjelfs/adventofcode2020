module Day4
  ( partOne,
    partTwo,
  )
where

import Data.List.Split (linesBy, splitOn)
import qualified Data.Map as M

type Passport = M.Map String String

mandatoryFields :: [String]
mandatoryFields =
  [ "byr",
    "iyr",
    "eyr",
    "hgt",
    "hcl",
    "ecl",
    "pid"
  ]

parsePassport :: String -> Passport
parsePassport str = M.fromList $ keyVal <$> splitOn " " str
  where
    keyVal subStr =
      case splitOn ":" subStr of
        (k : v : _) -> (k, v)
        _ -> error "substring not in the expected format"

passportIsValid :: Passport -> Bool
passportIsValid passport =
  all (`M.member` passport) mandatoryFields

partOne :: IO [Passport]
partOne = do
  inp <- linesBy (== "") . lines <$> readFile "data/day4.txt"
  pure $ filter passportIsValid $ parsePassport . unwords <$> inp

partTwo :: IO Int
partTwo = undefined
