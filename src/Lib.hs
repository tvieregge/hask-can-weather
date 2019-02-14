{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Control.Exception
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO

dataFile :: IO (Either IOException TL.Text)
dataFile = try $ TLIO.readFile "./data/Xeng-daily-01011954-12311954.csv.1"

fileOpenFail :: IOException -> IO ()
fileOpenFail e = do
    putStrLn $ "Exception on opening data file: " ++ show e

onlyShowErr :: Show e => IO (Either e a) -> IO ()
onlyShowErr action = do
    result <- action
    case result of
        Left e -> print e
        Right _ -> return ()

-- TODO: move printing/exception handling to main
someFunc :: IO ()
someFunc = (onlyShowErr dataFile) >>= print
