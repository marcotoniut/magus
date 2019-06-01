{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, NoImplicitPrelude,
    RankNTypes, RecursiveDo, TupleSections, TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Magus.Party where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fix
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Either
import Data.Function (const, id, flip, ($), (.))
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
import qualified Prelude as P

type DLogin = (RestChan, Gateway, [ThreadIdType])

-- newtype PartyT t m a = PartyT { runPartyT :: DLogin -> (m a, DLogin) }
newtype PartyT t m a = PartyT { runPartyT :: DLogin -> m a }

mapPartyT :: (m a -> n b) -> PartyT t m a -> PartyT t n b
mapPartyT f m = PartyT $ \dis -> f $ runPartyT m dis
{-# INLINE mapPartyT #-}

liftPartyT :: m a -> PartyT t m a
liftPartyT m = PartyT (const m)
{-# INLINE liftPartyT #-}

instance (Functor m) => Functor (PartyT t m) where
  fmap f = mapPartyT $ fmap $ \ ~a -> f a
  {-# INLINE fmap #-}

instance Applicative m => Applicative (PartyT t m) where
  pure = liftPartyT . pure
  {-# INLINE pure #-}
  f <*> v = PartyT $ \dis -> runPartyT f dis <*> runPartyT v dis

-- | CHECK
instance (Alternative m) => Alternative (PartyT t m) where
    empty = liftPartyT empty
    {-# INLINE empty #-}
    m <|> n = PartyT $ \dis -> runPartyT m dis <|> runPartyT n dis
    {-# INLINE (<|>) #-}

instance Monad m => Monad (PartyT t m) where
  return = lift . return
  {-# INLINE return #-}
  m >>= k  = PartyT $ \dis -> do
      ~a <- runPartyT m dis
      runPartyT (k a) dis
      -- return b
  {-# INLINE (>>=) #-}
  fail msg = lift $ fail msg
  {-# INLINE fail #-}

instance MonadPlus m => MonadPlus (PartyT t m) where
  mzero       = lift mzero
  {-# INLINE mzero #-}
  m `mplus` n = PartyT $ \dis -> runPartyT m dis `mplus` runPartyT n dis
  {-# INLINE mplus #-}

instance MonadTrans (PartyT t) where
  -- lift = PartyT $ \_ -> m
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
    mfix f = PartyT $ \dis -> mfix $ \a -> runPartyT (f a) dis
    {-# INLINE mfix #-}

instance PerformEvent t m => PerformEvent t (PartyT t m) where
  type Performable (PartyT t m) = PartyT t (Performable m)
  performEvent_ e =
    PartyT $ \dis -> performEvent_ $ flip runPartyT dis <$> e
  performEvent  e =
    PartyT $ \dis -> performEvent  $ flip runPartyT dis <$> e

instance TriggerEvent t m => TriggerEvent t (PartyT t m) where
  newTriggerEvent = lift newTriggerEvent
  newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete = lift . newEventWithLazyTriggerWithOnComplete

instance PostBuild t m => PostBuild t (PartyT t m) where
  getPostBuild = lift getPostBuild


-- | Party
class DiscordParty t m where
  invite ::
      --  (RestChan, Gateway, [ThreadIdType])
    -- -> Event t Snowflake
       Event t Snowflake
    -> m (Event t (Snowflake, Player))
    -- -> m (Event t (Either RestCallException (Snowflake, Player)))
  -- discordLogin :: (RestChan, Gateway, [ThreadIdType])

  -- inviteM :: 
  --     (RestChan, Gateway, [ThreadIdType])
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
  invite e = do
    PartyT $ flip requestParticipantCache e

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

requestParticipantCache :: forall t m.
  ( FetchConstraints t m
  , MonadFix m
  ) => (RestChan, Gateway, [ThreadIdType])
    -- -> Dynamic t (M.Map Snowflake Player)
    -> Event t Snowflake
    -> m (Event t (Snowflake, Player))
-- requestParticipantCache dis d_m e = do
requestParticipantCache dis e = mdo
  let e_mp = attachWith (\m i -> (i, M.lookup i m)) (current d_m) e
  
  (f_p, e_kp) <- fmap fanEither . performEvent $ e_mp <&> \(i, mp) -> case mp of
    Nothing -> do
      ep <- liftIO $ fetchParticipant dis i
      pure $ (i,) <$> ep
    -- TODO Rid of unnecessary inserts
    Just p  -> do
      pure $ pure (i, p)

  d_m <- accumDyn (\m (i, p) -> M.insert i p m) M.empty e_kp

  pure e_kp

