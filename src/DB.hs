{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}

module DB
  ( Config (..),
    readReservationsFromDB,
    getReservedSeatsFromDB,
    saveReservation,
    ServiceAddress,
    findCaravan,
    reserveCaravan,
  )
where

import ApiModel
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader.Class (MonadReader (..), asks)
import Data.List (find)
import Data.Time
  ( FormatTime (..),
    ZonedTime (..),
    defaultTimeLocale,
    formatTime,
  )
import System.Directory (doesFileExist)
import System.FilePath ((</>))

data Config
  = Config
      { connStr :: String,
        svcAddr :: String
      }

rawFileNameForDate :: FormatTime t => t -> String
rawFileNameForDate = formatTime defaultTimeLocale "%F"

fileNameForReservation :: Reservation -> FilePath
fileNameForReservation = (<> ".txt") . rawFileNameForDate . date

readReservationsFromDB :: (MonadReader Config m, MonadIO m) => ZonedTime -> m [Reservation]
readReservationsFromDB d =
  -- Imagine that this queries a database table instead of reading from a file
  do
    dir <- asks connStr
    let fileName = dir </> rawFileNameForDate d <> ".txt"
    exists <- liftIO (doesFileExist fileName)
    if exists
      then read <$> liftIO (readFile fileName)
      else pure []

getReservedSeatsFromDB :: (MonadReader Config m, MonadIO m) => ZonedTime -> m Int
getReservedSeatsFromDB d = do
  reservations <- readReservationsFromDB d
  pure do foldr ((+) . quantity) 0 reservations

saveReservation :: (MonadReader Config m, MonadIO m) => Reservation -> m ()
saveReservation r =
  --Imagine that this inserts into a database table instead of writing to a file
  do
    dir <- asks connStr
    reservations <- readReservationsFromDB (date r)
    -- Use of `seq` as described in http://stackoverflow.com/a/2530948/126014
    let fileName = dir </> fileNameForReservation r
    length reservations `seq` liftIO (writeFile fileName $ show (r : reservations))

-- Caravan storage
caravanPool :: [Caravan]
caravanPool = map Caravan [4, 6, 8]

fileNameForCaravan :: ZonedTime -> FilePath
fileNameForCaravan = (<> ".caravan.txt") . rawFileNameForDate

type ServiceAddress = String

readReservedCaravans :: (MonadReader Config m, MonadIO m) => ZonedTime -> m [Caravan]
readReservedCaravans d =
  -- Imagine that this queries a web service instead of reading from a file
  do
    dir <- asks svcAddr
    let fileName = dir </> fileNameForCaravan d
    exists <- liftIO (doesFileExist fileName)
    if exists
      then read <$> liftIO (readFile fileName)
      else pure []

findCaravan :: (MonadReader Config m, MonadIO m) => Int -> ZonedTime -> m (Maybe Caravan)
findCaravan requestedCapacity d = do
  liftIO (putStrLn "Finding a caravan...")
  reservedCaravans <- readReservedCaravans d
  let availableCaravans = filter (`notElem` reservedCaravans) caravanPool
  pure do find (\c -> requestedCapacity <= caravanCapacity c) availableCaravans

reserveCaravan :: (MonadReader Config m, MonadIO m) => ZonedTime -> Caravan -> m ()
reserveCaravan d c =
  --Imagine that this updates a web service instead of writing to a file
  do
    dir <- asks svcAddr
    let fileName = dir </> fileNameForCaravan d
    caravans <- readReservedCaravans d
    -- Use of `seq` as described in http://stackoverflow.com/a/2530948/126014
    length caravans `seq` liftIO (writeFile fileName $ show (c : caravans))
