{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Lib where

import Graphics.Matplotlib

import Control.Exception
import Data.List
import System.Directory

import Data.Text.Encoding
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO

import Data.Vector (Vector)
import qualified Data.Vector as Vector

import qualified Data.ByteString as B

-- cassava
import Data.Csv

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Search as BLS

import Data.Ix

data Input = Input
    { averagingWindow :: Int
    , dirName :: FilePath
    }

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

-- autoformat really messes this up...
instance FromNamedRecord CsvItem where
    parseNamedRecord m =
        CsvItem <$> m .: "Year" <*> m .: "Month" <*> m .: "Day" <*>
        m .: encodeUtf8 "Max Temp (°C)"

data DisplayItem = DisplayItem
    { displayYear :: [Int]
    , displayValue :: [Float]
    } deriving (Show, Eq)

data DataItem = DataItem
    { dYear :: Int
    , dMonth :: Int
    , dDay :: Int
    , dMaxTemp :: Float
    } deriving (Eq, Show)

run :: Input -> IO ()
run (Input averagingWindow dirName) = do
    eitherFileNames <- tryLS dirName
    case eitherFileNames of
         Left e -> print ("Couldn't read directory: " ++ show e)
         Right fileNames -> do
            let csvFiles = filter (isSuffixOf ".csv") fileNames
            fileData <- sequence $ map (B.readFile . ((dirName ++ "/") ++)) csvFiles
            onscreen . makePlot $ processData averagingWindow fileData
            return ()

tryLS :: String -> IO (Either IOException [FilePath])
tryLS fileName = try $ listDirectory fileName

makePlot :: [DisplayItem] -> Matplotlib
makePlot displayData =
    rc "legend" @@ [o2 "fontsize" 18] % (foldr1 (%) $ map plotItem displayData) %
    addOptions
  where
    plotItem item = plot (displayYear item) (displayValue item)
    addOptions =
        (legend @@ [o2 "labels" (map show [January ..]), o2 "loc" "upper left"]) %
        grid True

processData :: Int -> [B.ByteString] -> [DisplayItem]
processData window files =
    map (dataPipeline window) . monthlyData $ fileData files

dataPipeline :: Int -> [DataItem] -> DisplayItem
dataPipeline window =
    movingAvg window . sortByYear . toDisplayItem . groupByYear

fileData :: [B.ByteString] -> Vector CsvItem
fileData fs = Vector.concat . map decodeFile $ map (BL.fromChunks . (: [])) fs

monthlyData :: Vector CsvItem -> [[DataItem]]
monthlyData files = map (monthsData $ removeBadRecords files) [January ..]

monthsData :: [DataItem] -> Month -> [DataItem]
monthsData xs m = filter (\i -> dMonth i == fromEnum m + 1) xs

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

groupByYear :: [DataItem] -> [[DataItem]]
groupByYear = groupBy (\a b -> dYear a == dYear b)

toDisplayItem :: [[DataItem]] -> DisplayItem
toDisplayItem [] = DisplayItem [] [] -- TODO: mempty?
toDisplayItem dataItems =
    DisplayItem (map (dYear . head) dataItems) $ map reduce dataItems
  where
    reduce lst = sum (map dMaxTemp lst) / fromIntegral (length lst)

sortByYear :: DisplayItem -> DisplayItem
sortByYear xs = unzipDI . sortBy (\x y -> compare (fst x) (fst y)) $ zipDI xs
  where
    zipDI di = zip (displayYear di) (displayValue di)
    unzipDI pairs =
        let unzipped = unzip pairs
         in uncurry DisplayItem unzipped

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
