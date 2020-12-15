module Day14 where

import Common (mapLines)
import qualified Data.Map as M
import Data.List (isPrefixOf, foldl')
import Data.Bits (clearBit, setBit, shift, (.&.))

data Instr = Mask String | Mem Int Int deriving Show

type State = (String, (M.Map Int Int))

initialMask :: String 
initialMask = replicate 36 'X'

partOne :: IO Int
partOne = sum . snd . foldl' applyInstructions (initialMask, M.empty) <$> mapLines parseInstr "data/day14.txt"
    where
        applyInstructions (_, register) (Mask m) = (m, register) 
        applyInstructions (mask, register) (Mem addr val) = (mask, M.insert addr (applyMask val mask) register)

partTwo :: IO Int
partTwo = sum . snd . foldl' applyInstructions (initialMask, M.empty) <$> mapLines parseInstr "data/day14.txt"
    where
        applyInstructions (_, register) (Mask m) = (m, register) 
        applyInstructions (mask, register) (Mem addr val) = 
            (mask, foldr (\a r -> M.insert a val r) register $ applyMask2 addr mask)

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

perms :: [Int] -> [[Int -> Int]]
perms indices = 
    fmap 
        (\i -> 
            fmap (\(idx, j) ->
                case (shift i (-j)) .&. 1 :: Int of 
                    0 -> flip clearBit idx 
                    1 -> flip setBit idx
                    _ -> (\n -> n)
            ) (zip indices [0..(length indices - 1)])
        ) [0..(2 ^ (length indices))-1]

applyMask2 :: Int -> String -> [Int]
applyMask2 n mask = 
    case perms $ floatingIndices mask of
        [] -> [unfloating mask]
        p -> fmap (\ops -> foldr ($) (unfloating mask) ops) p
    where 
        unfloating = fst . foldr 
            (\c (n', idx) -> 
                case c of 
                    '1' -> (n' `setBit` idx, idx + 1)
                    _ -> (n', idx + 1)
            ) (n, 0) 
        floatingIndices = 
            fst . foldr 
                (\c (indices, idx) -> 
                    case c of 
                        'X' -> (idx : indices, idx + 1)
                        _ -> (indices, idx + 1)
                ) ([], 0) 
