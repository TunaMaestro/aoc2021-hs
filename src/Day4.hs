{-# LANGUAGE ImportQualifiedPost #-}

module Day4 (solution, cardPoints, CardPoint (..), cardsMap, callCard, BingoCard (..), newCard, incEntry) where

import Control.Applicative ((<|>))
import Control.Arrow (second, (>>>))
import Data.Foldable
import Data.Function ((&))
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as Set
import Data.List.Split
import Data.Map.Strict qualified as Map
import Solution
import Text.Printf (printf)
import qualified Data.Maybe as Maybe

-- import Debug.Trace
trace :: b -> a -> a
trace _ = id


size :: Int
size = 5

solution :: Solution.Solution
solution = Map.fromList [("Part 1", show . part1 . parse), ("Part 2", show . part2 . parse)]

type BingoRow = IntMap.IntMap Int

type CardMap = IntMap.IntMap [CardPoint]

type Input = (Calls, [Card])

type Card = [[Int]]

type Calls = [Int]

type Score = Int

data BingoCard = BingoCard
  { definition :: Card,
    horizontal :: BingoRow,
    vertical :: BingoRow,
    positiveDiag :: Int,
    negativeDiag :: Int,
    callHistory :: [Int]
  }
  deriving (Eq)

instance Show BingoCard where
  show (BingoCard def horiz vert _ _ history) =
    let historySet = Set.fromList history
        red s = "\x1b[31m" ++ s ++ "\x1b[0m"
        format n =
          let s = printf "%2d" n
           in if n `Set.member` historySet then red s else s

        -- row and column sizes assumed to be 5x5
        formattedRows = map (map format) def
        horizCounts = [IntMap.findWithDefault 0 i horiz | i <- [0 .. size]]
        vertCounts = [IntMap.findWithDefault 0 i vert | i <- [0 .. size]]

        headerLine = "   " ++ unwords (map (printf "%2d") vertCounts)

        -- add row headers and format each row
        bodyLines =
          zipWith
            ( \i row' ->
                printf "%2d " (horizCounts !! i) ++ unwords row'
            )
            [0 ..]
            formattedRows
     in '\n' : unlines (headerLine : bodyLines)

newCard :: Card -> BingoCard
newCard card = BingoCard {definition = card, horizontal = IntMap.empty, vertical = IntMap.empty, positiveDiag = 0, negativeDiag = 0, callHistory = []}

isComplete :: BingoCard -> Bool
isComplete =
  -- [pure . positiveDiag, pure . negativeDiag, ] removed: diags dont count
  sequence [IntMap.elems . horizontal, IntMap.elems . vertical]
    >>> concat
    >>> any (== size)

data CardPoint = CardPoint {cardId :: Int, row :: Int, col :: Int, number :: Int}
  deriving (Eq, Show)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

cardPoints :: Int -> Card -> [(Int, CardPoint)]
cardPoints cardId' arr =
  let grid = [(x, y, num) | (y, row') <- enumerate arr, (x, num) <- enumerate row']
   in map (\(x, y, num) -> (num, CardPoint {cardId = cardId', row = y, col = x, number = num})) grid

cardsMap :: [Card] -> CardMap
cardsMap =
  enumerate
    >>> map (uncurry cardPoints)
    >>> concat
    >>> map (second pure)
    >>> IntMap.fromListWith (++)

parse :: String -> Input
parse x = case splitOn "\n\n" x of
  (inputs : cards) ->
    ( splitOn "," inputs
        & map read,
      map parseCard cards
    )
  _ -> error "Bad input"

parseCard :: String -> Card
parseCard =
  splitOn "\n"
    >>> map
      ( splitOn " "
          >>> filter (not . null)
          >>> map read
      )

callCard :: CardPoint -> BingoCard -> BingoCard
callCard CardPoint {row = r, col = c, number = number'} card@BingoCard {horizontal = h, vertical = v, positiveDiag = pd, negativeDiag = nd, callHistory = callSum'} =
  card
    { horizontal = incEntry r h,
      vertical = incEntry c v,
      positiveDiag = pd + (if r == c then 1 else 0),
      negativeDiag = nd + (if r + c == size then 1 else 0),
      callHistory = number' : callSum'
    }

incEntry :: IntMap.Key -> IntMap.IntMap Int -> IntMap.IntMap Int
incEntry = IntMap.alter (Just . maybe 1 (+ 1))

part1 :: Input -> Int
part1 = evalGame 
      >>> (\x -> trace (show $ length x) x)
      >>> map fst
      >>> Maybe.catMaybes
      >>> unwrap where
        unwrap (x: _) = x
        unwrap [] = error "Couldn't find a winner :("

part2 :: Input -> Int
part2 = evalGame 
      >>> (\x -> trace (show $ length x) x)
      >>> map fst
      >>> Maybe.catMaybes
      >>> last

evalGame :: Input -> [(Maybe Score, IntMap.IntMap BingoCard)]
evalGame (calls, cards) =
  let start = IntMap.fromList $ enumerate cards & map (second newCard)
      boundUpdate = updateWithCall $ cardsMap cards
      updator state call = trace ("!! Called: " ++ show call ++ "\n" ++ show state) (state & snd & boundUpdate call)
   in scanl updator (Nothing, start) calls

-- reduceByCall :: CardMap -> Calls -> Calls -> IntMap.IntMap BingoCard -> Either (IntMap.IntMap BingoCard) (Int, Calls)
-- reduceByCall _ _ [] e = Left e
-- reduceByCall cardMap pastCalls (call : calls) state = undefined

-- A number is called; returns the new state OR the completed card
updateWithCall :: CardMap -> Int -> IntMap.IntMap BingoCard -> (Maybe Score, IntMap.IntMap BingoCard)
updateWithCall cardMap call state =
  let recipients = concat $ IntMap.lookup call cardMap
   in foldl' reduceStep (Nothing, state) recipients
  where
    reduceStep :: (Maybe Score, IntMap.IntMap BingoCard) -> CardPoint -> (Maybe Score, IntMap.IntMap BingoCard)
    reduceStep (previousWinner, state') point
      = let next = updateCallMap point state'
          in
          (previousWinner <|> tryScoreWin (number point) next, IntMap.filter (not . isComplete) next)
        --updateCallMap point >>> tryScoreWin (number point)
        -- pickFirstWinner = first (previousWinner <|>)


updateCallMap :: CardPoint -> IntMap.IntMap BingoCard -> IntMap.IntMap BingoCard
updateCallMap point = IntMap.update (Just . callCard point) (cardId point)

checkWin :: IntMap.IntMap BingoCard -> Maybe BingoCard
checkWin = find isComplete

scoreWin :: Int -> BingoCard -> Int
scoreWin lastCall card = lastCall' * remainingScore
  where
    lastCall' = trace ("Last called: " ++ show lastCall) lastCall
    remainingScore = trace ("Card remaining score: " ++ show (scoreCard card)) scoreCard card

tryScoreWin :: Int -> IntMap.IntMap BingoCard -> Maybe Score
tryScoreWin lastCall cardMap = score <$> checkWin cardMap
  where
    score = scoreWin lastCall

scoreCard :: BingoCard -> Int
scoreCard card =
  sum (concat $ definition card) - sum (callHistory card)
