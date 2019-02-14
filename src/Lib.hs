{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Exception
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO

onlyShowErr :: (Either IOException TL.Text) -> IO ()
onlyShowErr result = do
    case result of
        Left e -> print ("An error occured: " ++ show e)
        Right file -> TLIO.writeFile "temp" file

someFunc :: String -> IO ()
someFunc fileName = do
    result <- try $ processData <$> TLIO.readFile fileName
    onlyShowErr result
    return ()

processData :: TL.Text -> TL.Text
processData = id
