module Day3 (solution, chooseDigit, toBin, part1) where

import qualified Data.Map.Strict as Map
import Solution
import Data.List.Split
import Control.Arrow ((>>>), first, second, Arrow ((***)))
import Data.List (transpose, partition)

import Debug.Trace

import Data.Bits
import Data.Function ((&))



solution :: Solution.Solution
solution = Map.fromList [("Part 1", show . part1 . parse), ("Part 2", show . part2 . parse)]

parse :: String -> [[Int]]
parse = splitOn "\n"
  >>> map (map (\x -> if x == '1' then 1 else 0))
  >>> filter (not . null)

sumRowwise :: [[Int]] -> [Int]
sumRowwise = map sum . transpose

chooseDigit :: Int -> Int -> Int
chooseDigit len count = if 2 * count > len then 1 else 0

toBin :: [Int] -> Int
toBin = foldr (\a b -> a + b*2) 0 . reverse

part1 :: [[Int]] -> Int
part1 grid =
  let len = length grid
      sums = sumRowwise grid
      digits = map (chooseDigit len) sums
      num = toBin digits in
        num * (num `xor` (1 `shiftL` width grid - 1))
      

width :: [[a]] -> Int
width = length . transpose

type MinMax a = a -> a -> a

filterNums :: Ord a => (a -> a -> a) -> [([Int], [Int])] -> [([Int], [Int])]
filterNums choose = partition (firstIsOne . snd)
  >>> (\(x, y) -> (fAndId length x, fAndId length y))
  >>> uncurry choose
  >>> snd

fAndId :: (a -> b) -> a -> (b, a)
fAndId f a = (f a, a)

firstIsOne :: [Int] -> Bool
firstIsOne (1 : _) = True
firstIsOne (0 : _) = False
firstIsOne l = error ("Input data was not correct; tried to partition an empty list: " ++ show l)

part2 :: [[Int]] -> Int
part2 grid = part2FindOne 0 grid * part2FindOne 1 grid


part2FindOne :: Int -> [[Int]] -> Int
part2FindOne wanted grid = solvePart2 wanted (zip grid grid)
  & toBin

solvePart2 :: Int -> [([Int], [Int])] -> [Int]
solvePart2 _ [] = error "poo poo"
solvePart2 _ [x] = fst x
solvePart2 wanted l = solvePart2 wanted $ filterNums wanted (map (second $ drop 1) (traceShowId l))
  


