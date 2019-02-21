{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Lib where

import Graphics.Matplotlib

import Control.Exception
import Data.List
import System.Directory

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

import qualified Data.ByteString as B

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

data DisplayItem = DisplayItem
    { displayYear :: Int
    , displayValue :: Float
    }

-- autoformat really messes this up...
instance FromNamedRecord CsvItem where
    parseNamedRecord m =
        CsvItem <$> m .: "Year" <*> m .: "Month" <*> m .: "Day" <*>
        m .: (encodeUtf8 "Max Temp (°C)")

data DataItem = DataItem
    { dYear :: Int
    , dMonth :: Int
    , dDay :: Int
    , dMaxTemp :: Float
    } deriving (Eq, Show)

tryRead :: String -> IO (Either IOException TL.Text)
tryRead fileName = try $ TLIO.readFile fileName

someFunc :: [String] -> IO ()
someFunc dirName = do
    fileNames <- listDirectory "./data"
    files <-
        sequence . map B.readFile . map ("./data/" ++) $
        filter (isSuffixOf ".csv") fileNames
    let mlineOptions = map plotAxis . xAxes . monthlyData $ fileData files
    onscreen $ foldr1 (%) mlineOptions
    -- putStrLn $ "month: " ++ (show xAxes)
    return ()
  where
    plotAxis xAxis = plot [1 .. (length xAxis)] xAxis @@ [o2 "linewidth" 2]

xAxes :: [[DataItem]] -> [[Float]]
xAxes = (fmap . fmap) displayValue . map avgByYear

fileData :: [B.ByteString] -> Vector CsvItem
fileData fs = Vector.concat . map decodeFile $ map (BL.fromChunks . (: [])) fs

monthlyData :: Vector CsvItem -> [[DataItem]]
monthlyData files = map (monthsData $ removeBadRecords files) [January ..]
    -- n + maxTemp i
    -- eitherFiles <- mapM tryRead fileNames
    -- let files = sequence eitherFiles
    -- case files of
    --     Left e -> print ("An error occured: " ++ show e)

avgByYear :: [DataItem] -> [DisplayItem]
avgByYear xs = map sumDataItems grouped
  where
    grouped = groupBy (\a b -> (dYear a) == (dYear b)) xs
    sumDataItems l@(y:ys) =
        DisplayItem (dYear y) $ foldr (\y z -> (dMaxTemp y) + z) 0 l
    sumDataItems _ = DisplayItem 0 0

decodeFile :: BL.ByteString -> Vector CsvItem
decodeFile bs =
    case decodeData of
        Right x -> x
        Left e -> Vector.empty --putStrLn "Decoding file failed"
  where
    decodeData = snd <$> decodeByName (snd $ BLS.breakOn "\"Date/Time" bs)

removeBadRecords :: Vector CsvItem -> [DataItem]
removeBadRecords = Vector.foldr go []
  where
    go r rs =
        case maxTemp r of
            Right temp -> DataItem (year r) (month r) (day r) temp : rs
            Left e -> rs

monthsData :: [DataItem] -> Month -> [DataItem]
monthsData xs m = filter (\i -> (dMonth i) == (fromEnum m) + 1) xs
