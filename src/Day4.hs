module Day4
  ( partOne,
    partTwo,
  )
where

import Data.List.Split (linesBy, splitOn)
import qualified Data.Map as M

type Passport = M.Map String String

mandatoryFields =
  [ ("byr", validNum (1920, 2002)),
    ("iyr", validNum (2010, 2020)),
    ("eyr", validNum (2020, 2030)),
    ("hgt", validHeight),
    ("hcl", validHairColour),
    ("ecl", validEyeColour),
    ("pid", validPassportId)
  ]

digits = ['0' .. '9']

hexchars = ['a' .. 'f']

validPassportId :: String -> Bool
validPassportId str = length str == 9 && all (`elem` digits) str

validEyeColour :: String -> Bool
validEyeColour str =
  str `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validHairColour :: String -> Bool
validHairColour str =
  case str of
    ('#' : col) -> length col == 6 && all (`elem` (digits <> hexchars)) col
    _ -> False

validHeight :: String -> Bool
validHeight str =
  case reverse str of
    ('m' : 'c' : num) -> validNum (150, 193) (reverse num)
    ('n' : 'i' : num) -> validNum (59, 76) (reverse num)
    _ -> False

validNum :: (Int, Int) -> String -> Bool
validNum (mn, mx) str =
  let year = read str
   in year >= mn && year <= mx

parsePassport :: String -> Passport
parsePassport str = M.fromList $ keyVal <$> splitOn " " str
  where
    keyVal subStr =
      case splitOn ":" subStr of
        (k : v : _) -> (k, v)
        _ -> error "substring not in the expected format"

partOneValidation :: Passport -> Bool
partOneValidation passport =
  all (\(f, _) -> f `M.member` passport) mandatoryFields

partTwoValidation :: Passport -> Bool
partTwoValidation passport =
  all
    ( \(f, validator) ->
        case M.lookup f passport of
          Just val -> validator val
          Nothing -> False
    )
    mandatoryFields

parseAndValidate :: (Passport -> Bool) -> IO [Passport]
parseAndValidate validator = do
  inp <- linesBy (== "") . lines <$> readFile "data/day4.txt"
  pure $ filter validator $ parsePassport . unwords <$> inp

partOne :: IO [Passport]
partOne = parseAndValidate partOneValidation

partTwo :: IO [Passport]
partTwo = parseAndValidate partTwoValidation
