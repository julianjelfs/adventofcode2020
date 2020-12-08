module Day8 where

import Common (mapLines)
import Data.List ((!!))
import qualified Data.Set as S

data Instr
  = Acc Int
  | Jump Int
  | NoOp Int
  deriving (Eq, Ord, Show)

parseInstruction :: String -> Instr
parseInstruction str = instr num
  where
    instr =
      case take 3 str of
        "acc" -> Acc
        "jmp" -> Jump
        "nop" -> NoOp
        _ -> error "invalid instruction"
    num =
      case drop 4 str of
        ('+' : n) -> read n
        ('-' : n) -> negate $ read n
        _ -> error "invalid value"

runProgram :: Maybe Instr -> [Instr] -> (Int, Bool)
runProgram flip = go 0 0 S.empty
  where
    go acc i prev instr
      | i >= length instr = (acc, True) -- termination
      | otherwise =
        let curr' = instr !! i
            curr =
              if Just curr' == flip
                then flipInstruction curr'
                else curr'
            prev' = S.insert i prev
         in if S.member i prev
              then (acc, False) -- infinite loop
              else case curr of
                (Acc n) -> go (acc + n) (i + 1) prev' instr
                (Jump n) -> go acc (i + n) prev' instr
                (NoOp _) -> go acc (i + 1) prev' instr

flipInstruction :: Instr -> Instr
flipInstruction (Jump n) = NoOp n
flipInstruction (NoOp n) = Jump n
flipInstruction i = i

partOne :: IO (Int, Bool)
partOne = do
  instr <- mapLines parseInstruction "data/day8.txt"
  pure $ runProgram Nothing instr

partTwo :: IO [(Instr, (Int, Bool))]
partTwo = do
  instr <- mapLines parseInstruction "data/day8.txt"
  pure $ filter terminated $ withFlipped instr <$> instr
  where
    withFlipped instr i = (i, runProgram (Just i) instr)
    terminated (_, (_, term)) = term
