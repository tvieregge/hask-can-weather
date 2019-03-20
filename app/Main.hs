module Main where

import Data.Semigroup ((<>))
import Lib
import Options.Applicative
import System.Directory
import System.Environment (getArgs)

input :: Parser Input
input = Input
    <$> option
        auto
        (short 'w'
            <> value 1
            <> showDefault
            <> help "window size for moving average (1 disables)")
        <*> argument
            str
            (help "directory containing csv files" <> metavar "DIR")

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (input <**> helper) (fullDesc <> progDesc "Graphs weather data")
