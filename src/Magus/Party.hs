{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, NoImplicitPrelude,
    RankNTypes, RecursiveDo, ScopedTypeVariables, TupleSections, TypeFamilies #-}
module Magus.Party where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fix
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Either
import Data.Eq
import Data.Function (const, id, flip, ($), (.))
import Data.Functor ((<$>), (<&>), ($>))
import Data.Int
import Data.List (head)
import Data.Map (Map)
import Data.IntMap (IntMap, singleton, lookup)
import Data.Tuple (fst, uncurry)
import Discord (
    Snowflake, Gateway, ThreadIdType, RestChan
  , RestCallException, UserRequest(CreateDM, GetUser)
  , restCall
  )
import Prelude ((+))
import Reflex
import System.IO (IO)

import qualified Data.Map as M
import Magus.Types
import qualified Prelude as P

newtype PartyT t m a = PartyT
  { unPartyT
  :: Behavior t Int
  -> Event t (IntMap [(Snowflake, Either RestCallException Participant)])
  -> EventWriterT t (IntMap [Snowflake]) m a
  }

runPartyT :: forall t m w a.
  ( Monad m
  , MonadFix m
  , MonadHold t m
  , MonadIO (Performable m)
  , PerformEvent t m
  ) => (RestChan, Gateway, [ThreadIdType])
    -> PartyT t m a
    -> m a
runPartyT dis w = mdo
  d_id <- count e_req
  (a, e_req) <- runEventWriterT $ unPartyT w (current d_id) e_res
  e_res :: Event t (IntMap [(Snowflake, Either RestCallException Participant)]) <- performEvent $ e_req <&> \xs -> do -- \(i, k) -> do
    forM xs $ \ks -> do
      forM ks $ \k -> do
        res <- liftIO $ fetchParticipant dis k
        pure $ (k, res)
  pure a

-- TODO
runWithCachePartyT :: forall t m w a.
  ( Monad m
  , MonadFix m
  , MonadHold t m
  , MonadIO (Performable m)
  , PerformEvent t m
  ) => (RestChan, Gateway, [ThreadIdType])
    -> PartyT t m a
    -> m a
runWithCachePartyT = runPartyT

-- mapPartyT :: (m a -> n b) -> PartyT t m a -> PartyT t n b
mapPartyT :: (EventWriterT t (IntMap [Snowflake]) m a -> EventWriterT t (IntMap [Snowflake]) n b) -> PartyT t m a -> PartyT t n b
mapPartyT f m = PartyT $ \b e -> f $ unPartyT m b e
{-# INLINE mapPartyT #-}

liftPartyT :: (Reflex t, Monad m) => EventWriterT t (IntMap [Snowflake]) m a -> PartyT t m a
liftPartyT m = PartyT $ \b e -> m
{-# INLINE liftPartyT #-}

instance (Functor m) => Functor (PartyT t m) where
  fmap f = mapPartyT $ fmap $ \ ~a -> f a
  {-# INLINE fmap #-}

instance (Monad m, Reflex t, Applicative m) => Applicative (PartyT t m) where
  pure = liftPartyT . pure
  {-# INLINE pure #-}
  f <*> v = PartyT $ \b e -> liftA2 k (unPartyT f b e) (unPartyT v b e)
    where k ~a ~b = a b

-- | CHECK
-- instance (Monad m, Reflex t, Alternative m) => Alternative (PartyT t m) where
--   empty = lift empty
--   {-# INLINE empty #-}
--   m <|> n = PartyT $ \b e -> unPartyT m b e <|> unPartyT n b e
--   {-# INLINE (<|>) #-}

instance (Monad m, Reflex t) => Monad (PartyT t m) where
  return = lift . return
  {-# INLINE return #-}
  m >>= k  = PartyT $ \b e -> do
      ~a <- unPartyT m b e
      unPartyT (k a) b e
  {-# INLINE (>>=) #-}
  fail msg = lift $ fail msg
  {-# INLINE fail #-}

-- instance (Reflex t, MonadPlus m) => MonadPlus (PartyT t m) where
--   mzero       = lift mzero
--   {-# INLINE mzero #-}
--   m `mplus` n = PartyT $ \b e -> unPartyT m b e `mplus` unPartyT n b e
--   {-# INLINE mplus #-}

instance Reflex t => MonadTrans (PartyT t) where
  lift = liftPartyT . lift
  {-# INLINE lift #-}

instance (Reflex t, MonadIO m) => MonadIO (PartyT t m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

instance (MonadSample t m, Reflex t) => MonadSample t (PartyT t m) where
  sample = lift . sample

instance (MonadHold t m, Reflex t) => MonadHold t (PartyT t m) where
  hold a0 = lift . hold a0
  holdDyn a0 = lift . holdDyn a0
  holdIncremental a0 = lift . holdIncremental a0
  buildDynamic a0 = lift . buildDynamic a0
  headE = lift . headE

instance (MonadFix m, Reflex t) => MonadFix (PartyT t m) where
  mfix f = PartyT $ \b e -> mfix $ \a -> unPartyT (f a) b e
  {-# INLINE mfix #-}

instance (PerformEvent t m, Reflex t) => PerformEvent t (PartyT t m) where
  -- type Performable (PartyT t m) = PartyT t (Performable m)
  type Performable (PartyT t m) = Performable m
  performEvent_ = lift . performEvent_
  performEvent  = lift . performEvent

instance (Reflex t, TriggerEvent t m) => TriggerEvent t (PartyT t m) where
  newTriggerEvent = lift newTriggerEvent
  newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete = lift . newEventWithLazyTriggerWithOnComplete

instance PostBuild t m => PostBuild t (PartyT t m) where
  getPostBuild = lift getPostBuild

class (Monad m, Reflex t) => DiscordParty t m where
  invite :: Event t [Snowflake] -> m (Event t (Map Snowflake (Either RestCallException Participant)))

inviteOne :: DiscordParty t m => Event t Snowflake -> m (Event t (Snowflake, Either RestCallException Participant))
inviteOne e_i = fmap (head . M.toList) <$> invite (e_i <&> \s -> [s])

type FetchConstraints t m =
  ( MonadHold t m
  , MonadIO m
  , MonadIO (Performable m)
  , PerformEvent t m
  )

instance (FetchConstraints t m, MonadFix m) => DiscordParty t (PartyT t m) where
  invite e_k = do
    PartyT $ \b_id e_res -> do
      let e_ik = attach b_id e_k

      -- TODO Tell Map
      tellEvent (uncurry singleton <$> e_ik)

      b_k <- hold (-1) (fst <$> e_ik)
      let e_r :: Event t (Int, IntMap [(Snowflake, Either RestCallException Participant)]) = attachWith (,) b_k e_res
      -- let e_r :: Event t (Int, IntMap [(Snowflake, Either RestCallException Participant)]) = attachWith (,) b_k e_res
      -- pure $ Reflex.mapMaybe (\(i, (k, s, p)) -> if i == k then Just (s, p) else Nothing) e_r
      pure $ mapMaybe (\(i, xs) -> M.fromList <$> lookup i xs) e_r

fetchParticipant ::
       (RestChan, Gateway, [ThreadIdType])
    -> Snowflake
    -> IO (Either RestCallException Participant)
fetchParticipant dis i = do
  eu <- restCall dis (GetUser i)
  ec <- restCall dis (CreateDM i)
  pure $ Participant <$> eu <*> ec
