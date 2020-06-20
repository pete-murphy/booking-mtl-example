{-# LANGUAGE FlexibleContexts #-}

module App
  ( postReservation,
  )
where

import ApiModel
import Control.Monad (forM_)
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader.Class (MonadReader (..))
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import DB

hoistEither :: (MonadError e m) => Either e a -> m a
hoistEither = either throwError pure

{-
checkCaravan :: Reservation -> Error -> EitherT Error IO Reservation
checkCaravan reservation err = do
  c <- findCaravan svcAddr (quantity reservation) (date reservation)
  newRes <- hoistEither $ checkCaravanCapacityOnError err c reservation
  forM_ c $ reserveCaravan svcAddr (date newRes)
  return newRes
  -}

postReservation :: (MonadReader Config m, MonadIO m) => ReservationRendition -> m (HttpResult ())
postReservation candidate =
  fmap toHttpResult
    $ runExceptT
    $ do
      r <- hoistEither $ validateReservation candidate
      i <- getReservedSeatsFromDB $ date r
      hoistEither $ checkCapacity 10 i r
      >>= saveReservation
