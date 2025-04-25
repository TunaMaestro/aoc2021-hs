module Lib
  ( runDay,
    readDayFile,
    InputType(..),
  )
where

import Day3
import Solution
import Text.Printf (printf)

data InputType = Example | Input
  deriving (Eq, Ord, Show, Enum, Bounded)

inputFolder :: InputType -> String
inputFolder Example = "examples"
inputFolder Input = "inputs"

type Day = String

readDayFile :: InputType -> Day -> IO String
readDayFile inputType day = readFile ("puzzles/" ++ inputFolder inputType ++ ('/' : printf "%02s" day ++ ".txt"))

runDay :: Day -> Maybe Solution
runDay day = case intDay of
  3 -> Just Day3.solution
  _ -> Nothing
  where
    intDay = read day :: Int
