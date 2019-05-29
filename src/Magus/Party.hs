{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, NoImplicitPrelude,
    RankNTypes, RecursiveDo, TupleSections, TypeFamilies #-}
module Magus.Party where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fix
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Either
import Data.Function (id, ($), (.))
import Data.Functor ((<$>), (<&>))
import Data.Maybe
import Discord (
    Snowflake, Gateway, ThreadIdType, RestChan
  , RestCallException, UserRequest(CreateDM, GetUser)
  , restCall
  )
import Reflex
import System.IO (IO)

import qualified Data.Map as M
import Magus.Types

newtype PartyT t m a = PartyT { runPartyT :: m a }

mapPartyT :: (m a -> n b) -> PartyT t m a -> PartyT t n b
mapPartyT f m = PartyT $ f (runPartyT m)
{-# INLINE mapPartyT #-}

liftPartyT :: m a -> PartyT t m a
liftPartyT m = PartyT m

instance (Functor m) => Functor (PartyT t m) where
  fmap f = mapPartyT $ fmap $ \ ~a -> f a
  {-# INLINE fmap #-}

instance Applicative m => Applicative (PartyT t m) where
  pure = PartyT . pure
  f <*> v = PartyT $ liftA2 k (runPartyT f) (runPartyT v)
    where k ~a ~b = a b

-- | CHECK
instance (Alternative m) => Alternative (PartyT t m) where
    empty   = liftPartyT empty
    {-# INLINE empty #-}
    m <|> n = PartyT $ runPartyT m <|> runPartyT n
    {-# INLINE (<|>) #-}

instance Monad m => Monad (PartyT t m) where
  return = PartyT . return
  {-# INLINE return #-}
  m >>= k  = PartyT $ do
      ~a <- runPartyT m
      ~b <- runPartyT (k a)
      return b
  {-# INLINE (>>=) #-}
  fail msg = PartyT $ fail msg
  {-# INLINE fail #-}

instance MonadPlus m => MonadPlus (PartyT t m) where
  mzero       = PartyT mzero
  {-# INLINE mzero #-}
  m `mplus` n = PartyT $ runPartyT m `mplus` runPartyT n
  {-# INLINE mplus #-}

instance MonadTrans (PartyT t) where
  lift = liftPartyT
  {-# INLINE lift #-}

instance MonadIO m => MonadIO (PartyT t m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

instance MonadSample t m => MonadSample t (PartyT t m) where
  sample = lift . sample

instance MonadHold t m => MonadHold t (PartyT t m) where
  hold a0 = lift . hold a0
  holdDyn a0 = lift . holdDyn a0
  holdIncremental a0 = lift . holdIncremental a0
  buildDynamic a0 = lift . buildDynamic a0
  headE = lift . headE

instance (MonadFix m) => MonadFix (PartyT t m) where
    mfix f = PartyT $ mfix $ \a -> runPartyT (f a)
    {-# INLINE mfix #-}

instance PerformEvent t m => PerformEvent t (PartyT t m) where
  type Performable (PartyT t m) = PartyT t (Performable m)
  performEvent_ e = lift $ performEvent_ $ runPartyT <$> e
  performEvent  e = lift $ performEvent  $ runPartyT <$> e

instance TriggerEvent t m => TriggerEvent t (PartyT t m) where
  newTriggerEvent = lift newTriggerEvent
  newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete = lift . newEventWithLazyTriggerWithOnComplete

instance PostBuild t m => PostBuild t (PartyT t m) where
  getPostBuild = lift getPostBuild


-- | Party
class DiscordParty t m where
  invite ::
       (RestChan, Gateway, [ThreadIdType])
    -> Event t Snowflake
    -> m (Event t (Snowflake, Player))
    -- -> m (Event t (Either RestCallException (Snowflake, Player)))

-- TODO
-- inviteM :: 
--      (RestChan, Gateway, [ThreadIdType])
--   -> Event t [Snowflake]
--   -> m (Event t [(Snowflake, Player)])
-- D TODO
-- Traversable t


type FetchConstraints t m =
  ( MonadHold t m
  , MonadIO m
  , MonadIO (Performable m)
  , PerformEvent t m
  )

instance (FetchConstraints t m, MonadFix m) => DiscordParty t (PartyT t m) where
  invite dis = lift . requestParticipant dis

fetchParticipant ::
       (RestChan, Gateway, [ThreadIdType])
    -> Snowflake
    -> IO (Either RestCallException Player)
fetchParticipant dis i = do
  eu <- restCall dis (GetUser i)
  ec <- restCall dis (CreateDM i)
  pure $ Player <$> eu <*> ec

fetchParticipantEvent ::
  ( FetchConstraints t m
  ) => (RestChan, Gateway, [ThreadIdType])
    -> Event t Snowflake
    -> m (Event t (Either RestCallException Player))
fetchParticipantEvent dis e = performEvent $ liftIO . fetchParticipant dis <$> e

requestParticipant :: forall t m.
  ( FetchConstraints t m
  , MonadFix m
  ) => (RestChan, Gateway, [ThreadIdType])
    -> Event t Snowflake
    -> m (Event t (Snowflake, Player))
requestParticipant dis e = mdo
  let e_mp = attachWith (\m i -> (i, M.lookup i m)) (current d_m) e

  (f_p, e_kp) <- fmap fanEither . performEvent $ e_mp <&> \(i, mp) -> case mp of
    Nothing -> do
      ep <- liftIO $ fetchParticipant dis i
      pure $ (i,) <$> ep
    -- TODO Rid of unnecessary inserts
    Just p  -> pure $ pure (i, p)

  d_m <- accumDyn (\m (i, p) -> M.insert i p m) M.empty e_kp
  pure e_kp