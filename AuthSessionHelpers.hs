{-# LANGUAGE OverloadedStrings #-}

module AuthSessionHelpers where

import Crypto.Scrypt (EncryptedPass(..), encryptPassIO, defaultParams, getEncryptedPass, Pass(..), verifyPass)
import Data.Time.Clock (addUTCTime, getCurrentTime, nominalDay, UTCTime)
import Hasql.Pool (Pool)
import Web.Scotty (ActionM, liftAndCatchIO)

import DBHelpers (scottyDoesDB)
import DBTypes (addRow)
import qualified DBTypes.Account as Account (Row(..))
import qualified DBTypes.AuthSession as AuthSession (Row(..))
import qualified UnambiguiousStrings as US
import UUIDHelpers (asPassword, randomUUID)

makeNewUserSession :: Pool -> US.LText -> ActionM (AuthSession.Row, US.SBytes)
makeNewUserSession connections newUsername = do
  account <- makeNewUser connections newUsername
  (authSession, secret) <- makeSessionForUser connections account
  return (authSession, secret)

makeSessionForUser :: Pool -> Account.Row -> ActionM (AuthSession.Row, US.SBytes)
makeSessionForUser connections account = do
  time <- liftAndCatchIO getCurrentTime
  secret <- fmap asPassword $ liftAndCatchIO randomUUID
  hashedSecret <- fmap getEncryptedPass $ liftAndCatchIO $ encryptPassIO defaultParams (Pass secret)
  authID <- liftAndCatchIO randomUUID
  let authSession = AuthSession.Row {
    AuthSession.identifier = authID,
    AuthSession.account = Account.identifier account,
    AuthSession.hash = hashedSecret,
    AuthSession.expires = addUTCTime nominalDay time
    }
  scottyDoesDB connections $ addRow authSession
  return (authSession, secret)

makeNewUser :: Pool -> US.LText -> ActionM Account.Row
makeNewUser connections username = do
  newID <- liftAndCatchIO randomUUID
  let row = Account.Row {
    Account.identifier = newID,
    Account.name = US.toStrictText username
    }
  scottyDoesDB connections $ addRow row
  return row

hashPassword :: US.SBytes -> IO US.SBytes
hashPassword = (fmap getEncryptedPass) . (encryptPassIO defaultParams) . Pass

checkPassword :: US.SBytes -> US.SBytes -> Bool
checkPassword hash secret = fst $ verifyPass defaultParams (Pass secret) (EncryptedPass hash)
