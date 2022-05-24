{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Text.Chart (height, options, plot, plotWith)
import Data.Time
import Data.Time.Format
import Data.Vector
import qualified Data.Vector as V
import Entry
import System.Exit
import System.Process
import Text.RawString.QQ

mainQuestion :: IO ()
mainQuestion =
  putStrLn
    [r|What do you want to do next?
- Add today's weight? (a)
- Show stats? (stats)
- Show past entries (show)
- Show graphs (g)
- Quit (q)
|]

decodeEntries :: BL.ByteString -> [Entry]
decodeEntries csvString = case decodeByName csvString of
  Left err -> []
  Right (_, v) -> V.toList . V.map (\e -> Entry{date = date e, weight = weight e}) $ v

nextAction :: String -> [Entry] -> IO ()
nextAction "a" entries = putStrLn "Will save today\'s entry"
nextAction "show" entries = showPastEntries entries
nextAction "stats" entries = stats entries
nextAction "q" entries = putStrLn "Quiting..." >> exitSuccess
nextAction "g" entries = graphs entries
nextAction _ _ = putStrLn "Wrong selection"

graphs :: [Entry] -> IO ()
graphs entries =
  do
    system "clear"
    putStrLn "Will show graphs"
    plotWith options{height = 5} . fmap (round . weight) $ entries
    plotWith options{height = 5} . fmap (round . weight) $ entries
    plotWith options{height = 5} . fmap (round . weight) $ entries
    putStrLn "Press any key to go back."
    getLine
    mainLoop entries

stats :: [Entry] -> IO ()
stats entries =
  do
    system "clear"
    putStrLn $ stringifyStats (calcStats entries)
    putStrLn "Press any key to go back."
    getLine
    mainLoop entries

showPastEntries :: [Entry] -> IO ()
showPastEntries entries =
  do
    system "clear"
    putStrLn "How many entries do you want to see?"
    numOfEntries <- getLine
    Control.Monad.forM_ (takeLastN (read numOfEntries :: Int) entries) print
    putStrLn "Press any key to go back."
    getLine
    mainLoop entries

mainLoop :: [Entry] -> IO ()
mainLoop entries =
  forever $ do
    system "clear"
    mainQuestion
    selection <- getLine
    nextAction selection entries

main :: IO ()
main = do
  csvData <- BL.readFile "./data/data.csv"
  let entries = decodeEntries csvData
  mainLoop entries
