{-# LANGUAGE FlexibleContexts, KindSignatures, NoImplicitPrelude, ScopedTypeVariables #-}
module Discord.Reflex where

import Control.Applicative (pure)
import Control.Arrow (first)
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON)
import Data.Dependent.Sum
import Data.Either
import Data.Either.Combinators (rightToMaybe)
import Data.Function (id, ($), (.))
import Data.Functor (fmap, void, (<$>))
import Discord (Gateway, RestChan, ThreadIdType, nextEvent, restCall)
import Reflex

import qualified Discord as D
import qualified Prelude as P

subscribeToDiscord ::
  ( Reflex t
  , MonadIO m
  , TriggerEvent t m
  ) => (RestChan, Gateway, [ThreadIdType])
    -> m (Event t D.Event)
subscribeToDiscord dis = do
  (e, eTrigger) <- first (mapMaybe id) <$> newTriggerEvent
  void . liftIO . forkIO . forever $ do
    de <- D.nextEvent dis
    eTrigger $ rightToMaybe de
  pure e

emitToDiscord ::
  ( FromJSON a
  , MonadIO m
  , MonadIO (Performable m)
  , PerformEvent t m  
  , Reflex t
  , D.Request (r a)
  ) => (RestChan, Gateway, [ThreadIdType])
    -> Event t (r a)
    -> m (Event t (Either D.RestCallException a))
emitToDiscord dis = performEvent . fmap (liftIO . restCall dis)
