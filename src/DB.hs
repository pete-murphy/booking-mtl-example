{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module DB
  ( Config (..),
    Operations (..),
    ServiceAddress,
    runProduction,
    Production,
  )
where

import ApiModel
import Control.Monad.Except (ExceptT (..), lift)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.State (State (..))
import Control.Natural (type (~>))
import Data.List (find)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Time
  ( FormatTime (..),
    ZonedTime (..),
    defaultTimeLocale,
    formatTime,
  )
import Data.Time.Clock (UTCTime (..))
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

class Monad m => Operations m where
  readReservations :: ZonedTime -> m [Reservation]
  getReservedSeats :: ZonedTime -> m Int
  saveReservation :: Reservation -> m ()
  findCaravan :: Int -> ZonedTime -> m (Maybe Caravan)
  reserveCaravan :: ZonedTime -> Caravan -> m ()
  readReservedCaravans :: ZonedTime -> m [Caravan]

type Production = ReaderT Config IO

runProduction :: Config -> Production ~> IO
runProduction = flip runReaderT

data TestState
  = TestState
      { reservations :: Map UTCTime [Reservation],
        caravans :: Map UTCTime [Caravan]
      }

type Test = State TestState

instance Operations Production where
  readReservations d =
    -- Imagine that this queries a database table instead of reading from a file
    do
      dir <- asks connStr
      let fileName = dir </> rawFileNameForDate d <> ".txt"
      exists <- liftIO (doesFileExist fileName)
      if exists
        then read <$> liftIO (readFile fileName)
        else pure []
  getReservedSeats d = do
    reservations <- readReservations d
    pure do foldr ((+) . quantity) 0 reservations
  saveReservation r =
    --Imagine that this inserts into a database table instead of writing to a file
    do
      dir <- asks connStr
      reservations <- readReservations (date r)
      -- Use of `seq` as described in http://stackoverflow.com/a/2530948/126014
      let fileName = dir </> fileNameForReservation r
      length reservations `seq` liftIO (writeFile fileName $ show (r : reservations))
  readReservedCaravans d =
    -- Imagine that this queries a web service instead of reading from a file
    do
      dir <- asks svcAddr
      let fileName = dir </> fileNameForCaravan d
      exists <- liftIO (doesFileExist fileName)
      if exists
        then read <$> liftIO (readFile fileName)
        else pure []
  findCaravan requestedCapacity d = do
    liftIO (putStrLn "Finding a caravan...")
    reservedCaravans <- readReservedCaravans d
    let availableCaravans = filter (`notElem` reservedCaravans) caravanPool
    pure do find (\c -> requestedCapacity <= caravanCapacity c) availableCaravans
  reserveCaravan d c =
    --Imagine that this updates a web service instead of writing to a file
    do
      dir <- asks svcAddr
      let fileName = dir </> fileNameForCaravan d
      caravans <- readReservedCaravans d
      -- Use of `seq` as described in http://stackoverflow.com/a/2530948/126014
      length caravans `seq` liftIO (writeFile fileName $ show (c : caravans))

-- newtype ExceptT e m a = ExceptT {runExceptT :: m (Either e a)}
instance Operations m => Operations (ExceptT e m) where
  readReservations = lift . readReservations
  getReservedSeats = lift . getReservedSeats
  saveReservation = lift . saveReservation
  findCaravan = fmap lift . findCaravan
  reserveCaravan = fmap lift . reserveCaravan
  readReservedCaravans = lift . readReservedCaravans

-- instance (Monad m, MonadReader Config m, MonadIO m) => Operations m where
--   readReservations = readReservationsFromDB
--   getReservedSeats = getReservedSeatsFromDB
--   saveReservation = saveReservationToDB
--   findCaravan = findCaravanFromSvc
--   reserveCaravan = reserveCaravanFromSvc
--   readReservedCaravans = readReservedCaravansFromSvc

-- Caravan storage
caravanPool :: [Caravan]
caravanPool = map Caravan [4, 6, 8]

fileNameForCaravan :: ZonedTime -> FilePath
fileNameForCaravan = (<> ".caravan.txt") . rawFileNameForDate

type ServiceAddress = String
