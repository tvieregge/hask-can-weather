{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib where

import Control.Exception
import qualified Data.Foldable as Foldable
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO

data Item = Item
    { year :: Int
    , month :: Int
    , day :: Int
    , maxTemp :: Float
    }

tryRead :: String -> IO (Either IOException TL.Text)
tryRead fileName = try $ TLIO.readFile fileName

someFunc :: [String] -> IO ()
someFunc fileNames = do
    eitherFiles <- mapM tryRead fileNames
    let files = sequence eitherFiles
    case files of
        Left e -> print ("An error occured: " ++ show e)
        Right xs -> TLIO.writeFile "./data/temp" $ processData xs

processData :: [TL.Text] -> TL.Text
processData xs = head $ map (snd . TL.breakOn "Date/Time") xs
