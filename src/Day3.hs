{-# LANGUAGE ImportQualifiedPost #-}

module Day3 (solution, chooseDigit, toBin, part1) where

import Control.Arrow (second, (>>>))
import Data.Bifunctor qualified as Bifunctor
import Data.Bits
import Data.Function ((&))
import Data.List (partition, transpose)
import Data.List.Split
import Data.Map.Strict qualified as Map
import Debug.Trace
import Solution

solution :: Solution.Solution
solution = Map.fromList [("Part 1", show . part1 . parse), ("Part 2", show . part2 . parse)]

parse :: String -> [[Int]]
parse =
  splitOn "\n"
    >>> map (map (\x -> if x == '1' then 1 else 0))
    >>> filter (not . null)

sumRowwise :: [[Int]] -> [Int]
sumRowwise = map sum . transpose

chooseDigit :: Int -> Int -> Int
chooseDigit len count = if 2 * count > len then 1 else 0

toBin :: [Int] -> Int
toBin = foldr (\a b -> a + b * 2) 0 . reverse

part1 :: [[Int]] -> Int
part1 grid =
  let len = length grid
      sums = sumRowwise grid
      digits = map (chooseDigit len) sums
      num = toBin digits
   in num * (num `xor` (1 `shiftL` width grid - 1))

width :: [[a]] -> Int
width = length . transpose

data MinMax = Min | Max

filterNums :: MinMax -> [([Int], [Int])] -> [([Int], [Int])]
filterNums choose =
  partition (firstIsOne . snd)
    >>> Bifunctor.bimap f f
    >>> uncurry
      ( case choose of
          Min -> min
          Max -> max
      )
    >>> snd
  where
    f = traceShowId . fAndId length

fAndId :: (a -> b) -> a -> (b, a)
fAndId f a = (f a, a)

firstIsOne :: [Int] -> Bool
firstIsOne (1 : _) = True
firstIsOne (0 : _) = False
firstIsOne l = error ("Input data was not correct; tried to partition an empty list: " ++ show l)

part2 :: [[Int]] -> Int
part2 grid = part2FindOne Min grid * part2FindOne Max grid

part2FindOne :: MinMax -> [[Int]] -> Int
part2FindOne wanted grid =
  solvePart2 wanted (zip grid grid)
    & toBin

solvePart2 :: MinMax -> [([Int], [Int])] -> [Int]
solvePart2 _ [] = error "poo poo"
solvePart2 _ [x] = fst x
solvePart2 wanted l = solvePart2 wanted $ map (second $ drop 1) (filterNums wanted l)
