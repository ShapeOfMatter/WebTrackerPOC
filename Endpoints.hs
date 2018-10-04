{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Endpoints where

import Control.Monad ((>>=))
-- import Control.Monad.Fail (fail) -- We'll just use the Prelue `fail` for now.
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), maybeToExceptT)
import Data.List (intersperse)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid ((<>))
import Data.Time.Clock (getCurrentTime, UTCTime)
import Hasql.Pool (Pool)
import Network.HTTP.Types.Status (paymentRequired402)
import Text.Printf (printf)
import Web.Scotty (ActionM, header, html, liftAndCatchIO, param, raise, status, text)
import Web.Scotty.Cookie (getCookie, setSimpleCookie)

import AuthSessionHelpers (checkPassword, makeNewUserSession, makeSessionForUser)
import DBHelpers (dbPool, scottyActionFromEitherError, scottyDoesDB, scottyGuarenteesDB)
import DBTypes (addRow, getAllRows, getRow)
import qualified DBTypes.Account as Account (identifier, name, PrimaryKey(..), Row(..))
import qualified DBTypes.AuthSession as AuthSession (identifier, PrimaryKey(..), Row(..))
import qualified DBTypes.Consumption as Consumption (consumer, Row(..))
import qualified UnambiguiousStrings as US
import UUIDHelpers (fromSText, toSText, UUID)

homepage :: Pool -> String -> ActionM ()
homepage connections baseURL = do
  (accounts :: [Account.Row]) <- scottyDoesDB connections getAllRows
  (consumptions :: [Consumption.Row]) <- scottyDoesDB connections getAllRows
  let balance account = length [c | c <- consumptions, Consumption.consumer c == Account.identifier account]
  let pageRow account = US.packSText $ (printf "\"%s\":  %d") (Account.name account) (balance account)
  let accountNames = mconcat $ intersperse ",\n" $ fmap pageRow accounts
  template <- liftAndCatchIO $ readFile "Views/Homepage.html"
  let response = (printf template) baseURL accountNames
  html $ US.packLText response

handleLogin :: Pool -> ActionM ()
handleLogin connections = do
  (username :: US.SText) <- param "username"
  (maybeExistingUser :: Maybe Account.Row) <- fmap listToMaybe $ fmap (filter((username ==) . Account.name)) $ scottyDoesDB connections getAllRows
  ((newAuthSession, key) :: (AuthSession.Row, US.SBytes)) <- maybe
                                                                (makeNewUserSession connections $ US.fromStrictText username)
                                                                (makeSessionForUser connections)
                                                                maybeExistingUser
  setSimpleCookie "authID" $ toSText $ AuthSession.identifier newAuthSession
  either
    (raise . US.packLText . show)
    (setSimpleCookie "authKey")
    (US.strictDecodeEither key)
  template <- liftAndCatchIO $ readFile "Views/Success.html"
  html $ US.packLText template

noteConsumption :: Pool -> ActionM ()
noteConsumption connections = do
  recievedAt <- liftAndCatchIO getCurrentTime
  eConsumption <- runExceptT $ do
    let exceptMaybe e = (maybeToExceptT e) . MaybeT
    referer <- fmap US.toStrictText $ exceptMaybe "Unable to find a 'Referer' header." $ header "Referer"
    textAuthID <- exceptMaybe "Unable to find a 'authID' cookie." $ getCookie "authID"
    authID <- maybe (fail "The provided authID was not a valid UUID.") return (fromSText textAuthID)
    authKey <- fmap US.strictEncode $ exceptMaybe "Unable to find a 'authKey' cookie." $ getCookie "authKey"
    unAuthSession <- exceptMaybe "No such AuthSession" $ scottyDoesDB connections $ getRow $ AuthSession.PrimaryKey authID
    authSession <- if (checkPassword (AuthSession.hash unAuthSession) authKey) then return unAuthSession else fail "BAD KEY!"
    return Consumption.Row {
                        Consumption.consumer = AuthSession.account authSession,
                        Consumption.item = referer,
                        Consumption.happened = recievedAt
                      }
  let setCORSHeaders = do
                         setHeader "Access-Control-Allow-Origin" "*"
                         setHeader "Access-Control-Allow-Methods" "GET"
  let pleasePay e = do
                      status paymentRequired402
                      text e
  let saveAndRespond consumption = do
                                     scottyDoesDB connections $ addRow consumption
                                     user <- scottyGuarenteesDB connections $ getRow $ Account.PrimaryKey $ Consumption.consumer consumption
                                     text $ US.fromStrictText $ Account.name user
  setCORSHeaders
  either pleasePay saveAndRespond eConsumption

