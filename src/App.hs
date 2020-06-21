{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}

module App
  ( postReservation,
  )
where

import ApiModel
import Control.Monad ((<=<), forM_)
import Control.Monad.Except (ExceptT (..), MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..), liftIO)
import Control.Monad.Reader (MonadReader (..), runReaderT)
import DB

hoistEither :: (MonadError e m) => Either e a -> m a
hoistEither = either throwError pure

exceptT :: Monad m => (a -> m c) -> (b -> m c) -> ExceptT a m b -> m c
exceptT f g = either f g <=< runExceptT

checkCaravan ::
  Operations m =>
  Reservation ->
  Error ->
  ExceptT Error m Reservation
checkCaravan reservation err = do
  c <- findCaravan (quantity reservation) (date reservation)
  newRes <- hoistEither do checkCaravanCapacityOnError err c reservation
  forM_ c do reserveCaravan (date newRes)
  pure newRes

postReservation ::
  Operations m =>
  ReservationRendition ->
  m (HttpResult ())
postReservation candidate =
  toHttpResult
    <$> runExceptT do
      r <- hoistEither (validateReservation candidate)
      i <- getReservedSeats (date r)
      res <- exceptT
        (checkCaravan r)
        pure
        do hoistEither (checkCapacity 10 i r)
      saveReservation res

config :: Config
config = Config {connStr = ".", svcAddr = "."}
