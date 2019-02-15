{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Exception
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO

onlyShowErr :: IO (Either IOException TL.Text) -> IO ()
onlyShowErr action = do
    result <- action
    case result of
        Left e -> print ("An error occured: " ++ show e)
        Right _ -> return ()

-- onlyText :: Either a TL.Text -> [Either a TL.Text] -> [Either a TL.Text]
-- onlyText (Left a) rest = rest
-- onlyText

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
