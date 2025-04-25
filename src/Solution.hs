module Solution (Solution, Runner) where

import qualified Data.Map.Strict as Map

type Runner = (String -> String)

type Solution = Map.Map String Runner
