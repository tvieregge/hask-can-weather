{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Lib where

import Graphics.Matplotlib

import Control.Exception

import qualified Data.Foldable as Foldable
import Data.Text.Encoding
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO

import Data.Vector (Vector)
import qualified Data.Vector as Vector

-- cassava
import Data.Csv -- ( DefaultOrdered(headerOrder)
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

import Data.Ix

data Month
    = January
    | February
    | March
    | April
    | May
    | June
    | July
    | August
    | September
    | October
    | November
    | December
    deriving (Eq, Ord, Enum, Bounded, Ix, Read, Show)

data CsvItem = CsvItem
    { year :: Int
    , month :: Int
    , day :: Int
    , maxTemp :: Either Field Float
    } deriving (Eq, Show)

data DataItem = DataItem
    { dYear :: Int
    , dMonth :: Int
    , dDay :: Int
    , dMaxTemp :: Float
    } deriving (Eq, Show)

-- autoformat really messes this up...
instance FromNamedRecord CsvItem where
    parseNamedRecord m =
        CsvItem <$> m .: "Year" <*> m .: "Month" <*> m .: "Day" <*>
        m .: (encodeUtf8 "Max Temp (°C)")

tryRead :: String -> IO (Either IOException TL.Text)
tryRead fileName = try $ TLIO.readFile fileName

someFunc :: [String] -> IO ()
someFunc fileNames = do
    csvData <- BL.readFile "./data/eng-daily-01011890-12311890.csv"
    let v = snd <$> decodeByName (snd $ BLS.breakOn "\"Date/Time" csvData)
    let defData = case v of
         Right x -> x
         Left e -> Vector.empty --putStrLn "Decoding file failed"
    let xAxes = map (monthsData $ cleanData defData) [January ..]
    let p = (\xAxis -> plot [1 .. (length xAxis)] xAxis @@ [o1 "go-", o2 "linewidth" 2])
    let mlineOptions = map p xAxes
    onscreen $ foldr1 (%) mlineOptions
    putStrLn $ "month: " ++ (show xAxes)
  where
    twoRights (Right (Right x)) = x -- TODO: This is terrible, deal with error case
    oneRight (Right x) = x -- TODO: This is terrible, deal with error case
    -- n + maxTemp i
    -- eitherFiles <- mapM tryRead fileNames
    -- let files = sequence eitherFiles
    -- case files of
    --     Left e -> print ("An error occured: " ++ show e)
    --     Right xs -> TLIO.writeFile "./data/temp" $ processData xs

-- processData :: BL.ByteString -> Either String (Vector CsvItem)
-- processData bs = snd <$> decodeByName (snd $ BLS.breakOn "\"Date/Time" bs)

cleanData :: Vector CsvItem -> [DataItem]
cleanData  = Vector.foldr go []
    where go r rs = case maxTemp r of
                      Right temp -> DataItem (year r) (month r) (day r) temp : rs
                      Left e -> rs

monthsData :: [DataItem] -> Month -> [Float]
monthsData vs m = map dMaxTemp filtered
  where
    filtered = filter (\i -> (dMonth i) == (fromEnum m) + 1) vs
