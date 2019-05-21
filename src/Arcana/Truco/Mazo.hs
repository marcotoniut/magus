{-# LANGUAGE FlexibleContexts, NoImplicitPrelude #-}
module Arcana.Truco.Mazo where

-- import Control.Monad.State
import Control.Lens
import Data.Int
import Data.List
import Prelude (minBound)
import System.Random
import System.Random.Shuffle hiding (shuffle)
-- import Control.Monad.Random.Class 

import Arcana.Truco.Types

consMazo :: [Card]
consMazo =  [Card x y | x <- [minBound..] , y <- [minBound..]]

shuffle :: RandomGen g => g -> [a] -> [a]
shuffle r xs = shuffle' xs (length xs) r

dealPlayer :: Int -> [Card] -> ([Card], [Card])
dealPlayer l cs = (take l cs, drop l cs)

-- deal :: (MonadState Game m, MonadRandom m) => m ()
-- deal = do
--   ms <- shuffle
--   let mss = chunksOf 3 ms
--   players %= zipWith (set cards) mss
--   players %= map (set down [])
