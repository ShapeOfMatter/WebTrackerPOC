{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Endpoints where

import Control.Monad ((>>=))
-- import Control.Monad.Fail (fail) -- We'll just use the Prelue `fail` for now.
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), maybeToExceptT)
import Data.List (intersperse)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid ((<>))
import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.UUID (fromText, toText, UUID)
import Hasql.Pool (Pool)
import Network.HTTP.Types.Status (paymentRequired402)
import Web.Scotty (ActionM, header, liftAndCatchIO, param, raise, status, text)
import Web.Scotty.Cookie (getCookie, setSimpleCookie)

import AuthSessionHelpers (makeNewUserSession, makeSessionForUser)
import DBHelpers (dbPool, scottyActionFromEitherError, scottyDoesDB, scottyGuarenteesDB)
import DBTypes (addRow, getAllRows, getRow)
import qualified DBTypes.Account as Account (name, PrimaryKey(..), Row(..))
import qualified DBTypes.AuthSession as AuthSession (identifier, PrimaryKey(..), Row(..))
import qualified DBTypes.Consumption as Consumption (Row(..))
import qualified UnambiguiousStrings as US

homepage :: Pool -> ActionM ()
homepage connections = do
  (accounts :: [Account.Row]) <- scottyDoesDB connections getAllRows
  let accountNames = mconcat $ intersperse ",\n" $ fmap (("\"" <>) . (<> "\"") . Account.name) accounts
  text $ US.fromStrictText accountNames

handleLogin :: Pool -> ActionM ()
handleLogin connections = do
  (username :: US.SText) <- param "username"
  (maybeExistingUser :: Maybe Account.Row) <- fmap listToMaybe $ fmap (filter((username ==) . Account.name)) $ scottyDoesDB connections getAllRows
  ((newAuthSession, key) :: (AuthSession.Row, US.SBytes)) <- maybe
                                                                (makeNewUserSession connections $ US.fromStrictText username)
                                                                (makeSessionForUser connections)
                                                                maybeExistingUser
  setSimpleCookie "authID" $ toText $ AuthSession.identifier newAuthSession
  either
    (text . US.packLText . show)
    (setSimpleCookie "authKey")
    (US.strictDecodeEither key)
  text $ US.fromStrictText username

noteConsumption :: Pool -> ActionM ()
noteConsumption connections = do
  let checkPassword = undefined -- Delete this!
  recievedAt <- liftAndCatchIO getCurrentTime
  eConsumption <- runExceptT $ do
    let exceptMaybe e = (maybeToExceptT e) . MaybeT
    referer <- fmap US.toStrictText $ exceptMaybe "Unable to find a 'Referer' header." $ header "Referer"
    textAuthID <- exceptMaybe "Unable to find a 'authID' cookie." $ getCookie "authID"
    authID <- maybe (fail "The provided authID was not a valid UUID.") return (fromText textAuthID)
    authKey <- fmap US.strictEncode $ exceptMaybe "Unable to find a 'authKey' cookie." $ getCookie "authKey"
    unAuthSession <- exceptMaybe "No such AuthSession" $ scottyDoesDB connections $ getRow $ AuthSession.PrimaryKey authID
    authSession <- if (checkPassword (AuthSession.hash unAuthSession) authKey) then return unAuthSession else fail "BAD KEY!"
    return Consumption.Row {
                        Consumption.consumer = AuthSession.account authSession,
                        Consumption.item = referer,
                        Consumption.happened = recievedAt
                      }
  let pleasePay e = do
                      status paymentRequired402
                      text e
  let saveAndRespond consumption = do
                                     scottyDoesDB connections $ addRow consumption
                                     user <- scottyGuarenteesDB connections $ getRow $ Account.PrimaryKey $ Consumption.consumer consumption
                                     text $ US.fromStrictText $ Account.name user
  either pleasePay saveAndRespond eConsumption

