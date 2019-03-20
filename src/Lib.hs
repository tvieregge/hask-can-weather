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

-- A list of values to be displayed.
data SubplotData = SubplotData
    { displayYears :: [Int]
    , displayValues :: [Float]
    } deriving (Show, Eq)

data DataItem = DataItem
    { dYear :: Int
    , dMonth :: Int
    , dDay :: Int
    , dMaxTemp :: Float
    } deriving (Eq, Show)

tryLS :: String -> IO (Either IOException [FilePath])
tryLS fileName = try $ listDirectory fileName

run :: Input -> IO ()
run (Input averagingWindow dirName) = do
    eitherFileNames <- tryLS dirName
    case eitherFileNames of
         Left e -> print ("Couldn't read directory: " ++ show e)
         Right fileNames -> do
            let csvFiles = filter (isSuffixOf ".csv") fileNames
            readData <- sequence $ map (B.readFile . ((dirName ++ "/") ++)) csvFiles
            onscreen . makePlot $ processData averagingWindow readData
            return ()

-- Make the output plots for matplotlib to show
makePlot :: [SubplotData] -> Matplotlib
makePlot displayData =
    rc "legend" @@ [o2 "fontsize" 18]
        % (foldr1 (%) $ map plotItem displayData)
        % (legend @@ [o2 "labels" (map show [January ..]), o2 "loc" "upper left"])
        % grid True
    where
        plotItem item = plot (displayYears item) (displayValues item)

-- Transform the raw files into the finale representation
processData :: Int -> [B.ByteString] -> [SubplotData]
processData window files =
    map (dataPipeline window) . separateByMonth . sanitizeData $ wrangleData files

-- Take a single months data and transform it to it's final representation
dataPipeline :: Int -> [DataItem] -> SubplotData
dataPipeline window =
    movingAvg window . sortByYear . toSubplotData . groupByYear

-- Do some wrangleing to get the types to line up
wrangleData :: [B.ByteString] -> Vector CsvItem
wrangleData fs = Vector.concat . map decodeFile $ map (BL.fromChunks . (: [])) fs

-- Remove bad entries while converting types
sanitizeData :: Vector CsvItem -> [DataItem]
sanitizeData = Vector.foldr go []
  where
    go r rs =
        case maxTemp r of
            Right temp -> DataItem (year r) (month r) (day r) temp : rs
            Left e -> rs

-- Split the data into groups based on it's month
separateByMonth :: [DataItem] -> [[DataItem]]
separateByMonth files = map (filterMonth files) [January ..]
    where
        filterMonth xs m = filter (\i -> dMonth i == fromEnum m + 1) xs

-- Calculate the average of the display values using a sliding window.
movingAvg :: Int -> SubplotData -> SubplotData
movingAvg k (SubplotData ys lst) =
    SubplotData (take resultLength ys) $
    map avg . take resultLength . map (take k) $ tails lst
  where
    avg xs = sum xs / fromIntegral k
    resultLength = length lst - k + 1

groupByYear :: [DataItem] -> [[DataItem]]
groupByYear = groupBy (\a b -> dYear a == dYear b)

toSubplotData :: [[DataItem]] -> SubplotData
toSubplotData [] = SubplotData [] [] -- TODO: mempty?
toSubplotData dataItems =
    SubplotData (map (dYear . head) dataItems) $ map reduce dataItems
  where
    reduce lst = sum (map dMaxTemp lst) / fromIntegral (length lst)

sortByYear :: SubplotData -> SubplotData
sortByYear xs = unzipDI . sortBy (\x y -> compare (fst x) (fst y)) $ zipDI xs
  where
    zipDI di = zip (displayYears di) (displayValues di)
    unzipDI pairs =
        let unzipped = unzip pairs
         in uncurry SubplotData unzipped

decodeFile :: BL.ByteString -> Vector CsvItem
decodeFile bs =
    case decodeData of
        Right x -> x
        Left e -> Vector.empty
  where
    decodeData = snd <$> decodeByName (snd $ BLS.breakOn "\"Date/Time" bs)

