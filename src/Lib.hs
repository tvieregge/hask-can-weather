{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Lib where

import Control.Exception

import qualified Data.Foldable as Foldable
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO

import Data.Vector (Vector)
import qualified Data.Vector as Vector

-- cassava
import Data.Csv-- ( DefaultOrdered(headerOrder)
    -- , FromField(parseField)
    -- , FromNamedRecord(parseNamedRecord)
    -- , Header
    -- , ToField(toField)
    -- , ToNamedRecord(toNamedRecord)
    -- , (.:)
    -- , (.=)
    -- )

-- import qualified Data.Csv as Cassava
import qualified Data.ByteString.Lazy as BL

newtype DefaultMin = DefaultMin Float deriving (Eq,Ord,Show,Num)

data Item = Item
    { year :: Int
    , month :: Int
    , day :: Int
    , maxTemp :: DefaultMin
    } deriving (Eq, Show)

-- autoformat really messes this up...
instance FromNamedRecord Item where
    parseNamedRecord m =
        Item <$> m .: "Year" <*> m .: "Month" <*> m .: "Day" <*>
        m .: "Max Temp (C)"

instance FromField DefaultMin where
    parseField s = case runParser (parseField s) of
                        Left err -> pure $ DefaultMin (-1000.0)
                        Right n -> pure $ DefaultMin n

tryRead :: String -> IO (Either IOException TL.Text)
tryRead fileName = try $ TLIO.readFile fileName

someFunc :: [String] -> IO ()
someFunc fileNames = do
    csvData <- BL.readFile "./data/temp"
    let v = processData csvData :: Either String (Vector Item)
    let summed = fmap (foldr summer (DefaultMin 0)) v
    putStrLn $ "Total atBats was: " ++ (show summed)
  where
    summer i n = n + maxTemp i
    -- eitherFiles <- mapM tryRead fileNames
    -- let files = sequence eitherFiles
    -- case files of
    --     Left e -> print ("An error occured: " ++ show e)
    --     Right xs -> TLIO.writeFile "./data/temp" $ processData xs

processData :: BL.ByteString -> Either String (Vector Item)
processData bs = snd <$> decodeByName bs
-- processData :: [TL.Text] -> TL.Text
-- processData xs = Cassava.decodeByName file
--   where
--     file = head $ map (snd . TL.breakOn "Date/Time") xs
