{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Control.Exception (finally)
import Data.Functor ((<$>))
import Data.Text (pack)
import Discord (Auth(Auth), loginRestGateway, stopDiscord)
import Reflex.Host.Basic (basicHostForever)
import System.Environment (getEnv)
import System.IO (IO, putStrLn)

import Magus.RPS (rpsApp)

main :: IO ()
main = do
  putStrLn "Starting Magus..."
  tok <- pack <$> getEnv "DISCORD_AUTH_TOKEN"
  dis <- loginRestGateway (Auth tok)
  (`finally` stopDiscord dis) (basicHostForever (rpsApp dis) :: IO ())
