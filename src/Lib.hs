{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Lib where

import Graphics.Matplotlib

import Control.Exception

import qualified Data.Foldable as Foldable
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import Data.Text.Encoding

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
import qualified Data.ByteString.Lazy.Search as BLS

newtype DefaultMin = DefaultMin Float deriving (Eq,Ord,Show,Num)

data Item = Item
    { year :: Int
    , month :: Int
    , day :: Int
    , maxTemp :: Either Field Float
    } deriving (Eq, Show)

-- autoformat really messes this up...
instance FromNamedRecord Item where
    parseNamedRecord m =
        Item <$> m .: "Year" <*> m .: "Month" <*> m .: "Day" <*>
        m .: (encodeUtf8 "Max Temp (°C)")

tryRead :: String -> IO (Either IOException TL.Text)
tryRead fileName = try $ TLIO.readFile fileName

someFunc :: [String] -> IO ()
someFunc fileNames = do
    csvData <- BL.readFile "./data/eng-daily-01011890-12311890.csv"
    let v = processData csvData :: Either String (Vector Item)
    let summed = fmap (foldr summer 0) v
    let jan = sequence . sequence $ (Vector.filter (\i -> (month i) == 1)) <$> v
    let t = fmap (Vector.map maxTemp) jan
    let t2 = (fmap . fmap) Vector.toList (fmap sequence t)
    let mlineOptions = plot [1,2,3] (take 3 (twoRights t2)) @@ [o1 "go-", o2 "linewidth" 2]
    onscreen mlineOptions
    putStrLn $ "month: " ++ (show t2)
  where
    summer i n = case maxTemp i of
                      (Right temp) -> n + temp
                      (Left _) -> n
    twoRights (Right (Right x)) = x -- TODO: This is terrible, deal with error case
    -- n + maxTemp i
    -- eitherFiles <- mapM tryRead fileNames
    -- let files = sequence eitherFiles
    -- case files of
    --     Left e -> print ("An error occured: " ++ show e)
    --     Right xs -> TLIO.writeFile "./data/temp" $ processData xs

processData :: BL.ByteString -> Either String (Vector Item)
processData bs = snd <$> decodeByName (snd $ BLS.breakOn "\"Date/Time" bs)
-- processData :: [TL.Text] -> TL.Text
-- processData xs = Cassava.decodeByName file
--   where
--     file = head $ map (snd . TL.breakOn "Date/Time") xs
