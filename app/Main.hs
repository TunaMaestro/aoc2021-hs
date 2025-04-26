module Main (main) where

import qualified Data.Map as Map
import Data.List (intercalate)
import System.Environment (getArgs)
import Text.Printf (printf)
import Data.Maybe (fromMaybe)
import Lib
import Control.Arrow ((>>>))


main :: IO ()
main = do
  args <- getArgs
  case args of
    (day : rest) -> do
      let inputTypes = case rest of
                           ["example"] -> [Example]
                           ["input"]   -> [Input]
                           _           -> [Example, Input]

      fileContents <- Map.fromList <$> mapM (\inputType -> do
                                                content <- readDayFile inputType day
                                                return (inputType, content)) inputTypes

      case runDay day of
        Nothing -> putStrLn "You bum, that's not implemented."
        Just solutions -> do
          let evaluated = Map.map (\s -> Map.map s (Map.fromList $ map (\inputType -> (inputType, fromMaybe "" $ Map.lookup inputType fileContents)) inputTypes)) solutions
              innerStringed = Map.mapWithKey (\k v -> k ++ ":\n" ++ innerString v) evaluated
           in putStrLn $ intercalate "\n" (Map.elems innerStringed)
    _ -> putStrLn "Please provide a day as an argument."

innerString :: Map.Map InputType String -> String
innerString =
  Map.mapWithKey (\typeKey typeV -> '\t' : printf "% -10s" (show typeKey ++ ": ") ++ typeV)
    >>> Map.elems
    >>> intercalate "\n"
