{-# LANGUAGE NoImplicitPrelude, DataKinds, LambdaCase, ViewPatterns, TypeFamilies, FunctionalDependencies #-}
module Discord.Reflex.Command where

import Control.Applicative (liftA2)
import Data.Bool (Bool, (||))
import Data.Eq ((==))
import Data.Function (($), (.))
import Data.Functor (fmap, (<$>))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Proxy (Proxy(Proxy))
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Text (pack, isPrefixOf)
import Data.Witherable (Filterable, mapMaybe, filter)
import Discord (Event(MessageCreate), Message, messageText, messageChannel)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import Discord.Reflex

filterMessageCreate :: Filterable f => f Event -> f Message
filterMessageCreate = mapMaybe $ \case
  MessageCreate m -> Just m
  _               -> Nothing

hasPrefix :: String -> Message -> Bool
hasPrefix (pack -> n) = liftA2 (||) (== n) (isPrefixOf (n <> pack " ")) . messageText

class (KnownSymbol n) => DiscordCommand (n :: Symbol) r | n -> r where
  catchCommand :: Filterable f => Proxy n -> f Event -> f r
  catchCommand p
    = fmap (parseCommand p)
    . filter (hasPrefix (symbolVal p)) -- TODO fmapMaybe consume prefix
    . filterMessageCreate
  parseCommand :: Proxy n -> Message -> r
