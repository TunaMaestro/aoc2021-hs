{-# LANGUAGE ImportQualifiedPost #-}

module Day4 (solution, cardPoints, CardPoint (..), cardsMap) where

import Control.Arrow (second, (***), (>>>))
import Data.Bifunctor qualified as Bifunctor
import Data.Bits
import Data.Function ((&))
import Data.IntMap.Strict qualified as IntMap
import Data.List (partition, transpose)
import Data.List.Split
import Data.Map.Strict qualified as Map
import Debug.Trace
import Solution

size = 5

solution :: Solution.Solution
solution = Map.fromList []

type BingoRow = IntMap.IntMap Int

data BingoCard = BingoCard
  { horizontal :: BingoRow,
    vertical :: BingoRow,
    positiveDiag :: Int,
    negativeDiag :: Int
  }
  deriving (Eq, Show)

isComplete :: BingoCard -> Bool
isComplete =
  sequence [pure . positiveDiag, pure . negativeDiag, IntMap.elems . horizontal, IntMap.elems . vertical]
    >>> concat
    >>> any (== size)

data CardPoint = CardPoint {cardId :: Int, row :: Int, col :: Int}
  deriving (Eq, Show)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

cardPoints :: Int -> [[Int]] -> [(Int, CardPoint)]
cardPoints cardId arr =
  let grid = [(x, y, num) | (y, row) <- enumerate arr, (x, num) <- enumerate row]
   in map (\(x, y, num) -> (num, CardPoint {cardId = cardId, row = y, col = x})) grid

cardsMap :: [[[Int]]] -> IntMap.IntMap [CardPoint]
cardsMap =
  enumerate
    >>> map (uncurry cardPoints)
    >>> concat
    >>> map (second pure)
    >>> IntMap.fromListWith (++)

type Input = ([Int], [[[Int]]])

parse :: String -> Input
parse x = case splitOn "\n\n" x of
  (inputs : cards) ->
    ( splitOn "," inputs
        & map read,
      map parseCard cards
    )
  _ -> error "Bad input"

parseCard :: String -> [[Int]]
parseCard =
  splitOn "\n"
    >>> map
      ( splitOn " "
          >>> map read
      )

part1 :: Input -> Int
part1 x = let cm = cardsMap x in
  
