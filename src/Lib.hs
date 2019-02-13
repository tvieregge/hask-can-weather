{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO

dataFile :: IO TL.Text
dataFile =
    TLIO.readFile "./data/eng-daily-01011954-12311954.csv.1"

someFunc :: IO ()
someFunc = dataFile >>= print
