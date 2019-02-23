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
    { displayYear :: [Int]
    , displayValue :: [Float]
    } deriving (Show)

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
    let groupedData =
            map ((movingAvg 4) . sortByYear . toDisplayItem . groupByYear) . monthlyData $ fileData files
    let mlineOptions =
            map plotAxis $
            map (\xs -> zip (displayYear xs) (displayValue xs)) groupedData
    onscreen $ (foldr1 (%) mlineOptions) % grid True
    return ()
  where
    plotAxis items = plot ys xs
      where
        (ys, xs) = unzip items

fileData :: [B.ByteString] -> Vector CsvItem
fileData fs = Vector.concat . map decodeFile $ map (BL.fromChunks . (: [])) fs

monthlyData :: Vector CsvItem -> [[DataItem]]
monthlyData files = map (monthsData $ removeBadRecords files) [January ..]

monthsData :: [DataItem] -> Month -> [DataItem]
monthsData xs m = filter (\i -> (dMonth i) == (fromEnum m) + 1) xs
    -- n + maxTemp i
    -- eitherFiles <- mapM tryRead fileNames
    -- let files = sequence eitherFiles
    -- case files of
    --     Left e -> print ("An error occured: " ++ show e)

movingAvg :: Int -> DisplayItem -> DisplayItem
movingAvg k (DisplayItem ys lst) =
    DisplayItem (take resultLength ys) $
    map avg . take resultLength . map (take k) $ tails lst
  where
    avg xs = sum xs / fromIntegral k
    resultLength = length lst - k + 1

mavg :: Fractional b => Int -> [b] -> [b]
mavg k lst = take (length lst - k) $ map average $ tails lst
  where
    average = (/ fromIntegral k) . sum . take k

-- sumDsp :: [DisplayItem] -> DisplayItem
-- sumDsp l@(y:ys) =
--     DisplayItem (displayYear y) $ foldr (\y z -> (displayValue y) + z) 0 l
-- sumDsp _ = DisplayItem 0 0

groupByYear :: [DataItem] -> [[DataItem]]
groupByYear xs = groupBy (\a b -> (dYear a) == (dYear b)) xs

toDisplayItem :: [[DataItem]] -> DisplayItem
toDisplayItem [] = DisplayItem [] [] -- TODO: mempty?
toDisplayItem dataItems =
    DisplayItem (map (dYear . head) dataItems) $ map reduce dataItems
    where
    reduce lst = (sum $ map dMaxTemp lst) / (fromIntegral $ length lst)

sortByYear :: DisplayItem -> DisplayItem
sortByYear xs = unzipDI . sortBy (\x y -> compare (fst x) (fst y)) $ zipDI xs
    where zipDI di = zip (displayYear di) (displayValue di)
          unzipDI pairs =
              let unzipped = unzip pairs
                  in DisplayItem (fst unzipped) (snd unzipped)

decodeFile :: BL.ByteString -> Vector CsvItem
decodeFile bs =
    case decodeData of
        Right x -> x
        Left e -> Vector.empty
  where
    decodeData = snd <$> decodeByName (snd $ BLS.breakOn "\"Date/Time" bs)

removeBadRecords :: Vector CsvItem -> [DataItem]
removeBadRecords = Vector.foldr go []
  where
    go r rs =
        case maxTemp r of
            Right temp -> DataItem (year r) (month r) (day r) temp : rs
            Left e -> rs
