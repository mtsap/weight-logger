{-# LANGUAGE DeriveGeneric #-}

module Entry (
  Entry (..),
  myFormatTime,
  myParseTime,
  stringifyStats,
  calcStats,
  takeLastN,
) where

import Data.ByteString.Char8 (pack, unpack)
import Data.Csv
import Data.Time
import Data.Time.Format
import GHC.Generics (Generic)

data Entry = Entry
  { weight :: !Float
  , date :: !UTCTime
  }
  deriving (Eq, Generic)

instance Show Entry where
  show (Entry a b) = "Date: " Prelude.++ myFormatTime b Prelude.++ "\tWeight: " Prelude.++ show a

instance FromNamedRecord Entry
instance ToNamedRecord Entry
instance FromField UTCTime where
  parseField = parseTimeM False defaultTimeLocale dateFormat . unpack
instance ToField UTCTime where
  toField = toField . formatTime defaultTimeLocale dateFormat

dateFormat = "%d/%m/%Y"

myParseTime :: String -> UTCTime
myParseTime = parseTimeOrError False defaultTimeLocale dateFormat

myFormatTime :: UTCTime -> String
myFormatTime = formatTime defaultTimeLocale dateFormat

stringifyStats :: (Float, Float, Float, Float) -> String
stringifyStats (lastEntry, weeklyAvg, biWeeklyAvg, monthlyAvg) =
  "\tLast Entry: \tWeekly Avg: \tBiweekly Avg: \tMonthly Avg:\nWeight:\t"
    ++ show lastEntry
    ++ " kg\t\t"
    ++ show weeklyAvg
    ++ " kg\t"
    ++ show biWeeklyAvg
    ++ " kg\t"
    ++ show monthlyAvg
    ++ " kg"

calcStats :: [Entry] -> (Float, Float, Float, Float)
calcStats [] = (0, 0, 0, 0)
calcStats entries = (lastEntry, weeklyAvg, biWeeklyAvg, monthlyAvg)
 where
  lastEntry = weight . last $ entries
  weeklyAvg = avg . map weight . takeLastN 7 $ entries
  biWeeklyAvg = avg . map weight . takeLastN 14 $ entries
  monthlyAvg = avg . map weight . takeLastN 30 $ entries

takeLastN :: Int -> [a] -> [a]
takeLastN n = Prelude.reverse . Prelude.take n . Prelude.reverse

avg :: (Fractional a) => [a] -> a
avg [] = error "Cannot take average of empty list"
avg nums =
  let (sum, count) = foldr (\e (s, c) -> (s + e, c + 1)) (0, 0) nums
   in sum / count
