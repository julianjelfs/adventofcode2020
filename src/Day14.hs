module Day14 where

import Common (mapLines)
import qualified Data.Map as M
import Data.List (isPrefixOf, foldl')
import Data.Bits (clearBit, setBit)

data Instr = Mask String | Mem Int Int deriving Show

type State = (String, (M.Map Int Int))

partOne :: IO Int
partOne = sum . snd . foldl' applyInstructions (initialMask, M.empty) <$> mapLines parseInstr "data/day14.txt"
    where
        applyInstructions (_, register) (Mask m) = (m, register) 
        applyInstructions (mask, register) (Mem addr val) = (mask, M.insert addr (applyMask val mask) register)
        initialMask = replicate 36 'X'

partTwo :: Int
partTwo = undefined

parseInstr :: String -> Instr
parseInstr str
    | "mask = " `isPrefixOf` str = Mask $ drop 7 str
    | "mem[" `isPrefixOf` str = Mem (addr str) (val str)
    | otherwise = error "unexpected input"
    where 
        addr = read . takeWhile (/= ']') . tail . dropWhile (/= '[')
        val = read . drop 2 . dropWhile (/= '=')

applyMask :: Int -> String -> Int
applyMask n =
    fst . foldr 
        (\c (n', idx) -> 
            case c of 
                '0' -> (n' `clearBit` idx, idx + 1)
                '1' -> (n' `setBit` idx, idx + 1)
                _ -> (n', idx + 1)
        ) (n, 0) 
