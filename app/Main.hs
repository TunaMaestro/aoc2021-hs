module Main (main) where

import Control.Arrow ((>>>))
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Lib
import System.Environment (getArgs)
import Text.Printf (printf)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (day : _) -> do
      f1 <- Lib.readDayFile Example day
      f2 <- Lib.readDayFile Input day
      let types = Map.fromList [(Example, f1), (Input, f2)]
      case runDay day of
        Nothing -> putStrLn "You bum, that's not implemented."
        Just solutions -> do
          let evaluated = Map.map (\s -> Map.map s types) solutions
           in let innerStringed = Map.mapWithKey (\k v -> k ++ ":\n" ++ innerString v) evaluated
               in putStrLn $ intercalate "\n" (Map.elems innerStringed)
    _ -> putStrLn "Please provide a day as an argument."

innerString :: Map.Map InputType String -> String
innerString =
  Map.mapWithKey (\typeKey typeV -> '\t' : printf "% -10s" (show typeKey ++ ": ") ++ typeV)
    >>> Map.elems
    >>> intercalate "\n"
