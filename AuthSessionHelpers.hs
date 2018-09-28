{-# LANGUAGE OverloadedStrings #-}

module AuthSessionHelpers where

import Data.UUID.V4 (nextRandom)
import Hasql.Pool (Pool)
import Web.Scotty (ActionM, liftAndCatchIO)

import DBHelpers (scottyDoesDB)
import DBTypes (addRow)
import qualified DBTypes.Account as Account (Row(..))
import qualified DBTypes.AuthSession as AuthSession (Row(..))
import qualified UnambiguiousStrings as US

makeNewUserSession :: Pool -> US.LText -> ActionM (AuthSession.Row, US.SBytes)
makeNewUserSession connections newUsername = undefined -- do
--  account <- makeNewUser connections newUsername
  

makeSessionForUser :: Pool -> Account.Row -> ActionM (AuthSession.Row, US.SBytes)
makeSessionForUser connections account = undefined

makeNewUser :: Pool -> US.LText -> ActionM Account.Row
makeNewUser connections username = do
  newID <- liftAndCatchIO nextRandom
  let row = Account.Row {
    Account.identifier = newID,
    Account.name = US.toStrictText username
    }
  scottyDoesDB connections $ addRow row
  return row

