{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ConfigFile (emptyCP, readfile, simpleAccess)
import Data.Monoid ((<>))
import Data.Tuple.Sequence (sequenceT)
import Hasql.Pool (release)
import System.Exit (die)
import Web.Scotty (ActionM, get, liftAndCatchIO, post, put, scotty)

import DBHelpers (dbPool)
import Endpoints (handleLogin, homepage, noteConsumption)

dieOnConfigError = let handleError cpError = die $ concat ["There was a config file error: ", show $ fst cpError, " ", snd cpError]
                   in either handleError return

configurationParser fileName = do
  eitherErrorParser <- readfile emptyCP fileName
  dieOnConfigError eitherErrorParser

main = do
  conf <- configurationParser "defaults.config"
  dbSettings <- let dbConf = dieOnConfigError . (simpleAccess conf "DatabaseConnectionPool")
                in  sequenceT (
                  dbConf "maxconnections",
                  dbConf "maxidleseconds",
                  dbConf "host",
                  dbConf "port",
                  dbConf "user",
                  dbConf "password",
                  dbConf "database"
                )
  pool <- dbPool dbSettings
  baseURL <- dieOnConfigError $ simpleAccess conf "SiteSettings" "baseurl"
  port <- fmap read $ dieOnConfigError $ simpleAccess conf "SiteSettings" "port" -- deliberatly unsafe.
  scotty port $ do
    get "/" $ homepage pool baseURL
    post "/login" $ handleLogin pool
    get "/consume" $ noteConsumption pool
  release pool -- should we do this? 


