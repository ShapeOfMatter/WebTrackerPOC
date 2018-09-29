{-# LANGUAGE OverloadedStrings #-}

module UUIDHelpers (
  UUID,
  module UUIDHelpers
) where

import Control.Arrow (left)
import Control.Monad.Fail (MonadFail)
import Data.Time.Clock (UTCTime)
--import Data.Time.Format.ISO8601 (iso8601Show)
import Data.UUID (fromText, toByteString, toText, UUID)
import Data.UUID.V4 (nextRandom)
import Data.UUID.V5 (generateNamed)
import Web.Scotty (Parsable(parseParam))

import qualified UnambiguiousStrings as US

instance Parsable UUID where
--parseParam :: US.LText -> Either US.LText UUID
  parseParam = (left US.fromStrictText) . fromSText . US.toStrictText

randomUUID :: IO UUID
randomUUID = nextRandom

--childUUIDFromTime :: UUID -> UTCTime -> UUID -- This does not guarentee good resolution or abscence of collisions.
--childFromTime parent time =
--  generateNamed parent $ US.unpackSBytes $ US.strictEncode $ US.packSText $ iso8601Show time

asPassword :: UUID -> US.SBytes
asPassword = US.toStrictBytes . toByteString

toSText :: UUID -> US.SText
toSText = toText

fromSText :: US.SText -> Either US.SText UUID
fromSText = (maybe (Left "Unable to parse UUID") Right) . fromText

