module Main where

import Data.Semigroup ((<>))
import Lib
import Options.Applicative
import System.Directory
import System.Environment (getArgs)

-- won't be using hindent for future projects, this is ugly...
input :: Parser Input
input =
    Input <$>
    argument
        auto
        (value 1 <> showDefault <>
         help "window size for moving average (1 disables)") <*>
    argument str (help "directory containing csv files")

-- main :: IO ()
-- main = greet =<< execParser opts
--   where
--     opts = info (sample <**> helper)
--       ( fullDesc
--      <> progDesc "Print a greeting for TARGET"
--      <> header "hello - a test for optparse-applicative" )
main :: IO ()
main = run =<< execParser opts
  where
    opts = info (input <**> helper) (fullDesc <> progDesc "Graphs weather data")
-- main = run 4 "./data"
-- main = do
--     args <- getArgs
--     case args ofrun 4 "./data"
