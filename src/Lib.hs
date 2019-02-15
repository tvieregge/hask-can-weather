{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Exception
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO

willIFail :: String -> IO (Either IOException TL.Text)
willIFail fileName = try $ TLIO.readFile fileName

someFunc :: [String] -> IO ()
someFunc fileNames = do
    eitherFiles <- mapM willIFail fileNames
    let files = sequence eitherFiles
    case files of
        Left e -> print ("An error occured: " ++ show e)
        Right xs -> print xs

processData :: TL.Text -> TL.Text
processData = id
