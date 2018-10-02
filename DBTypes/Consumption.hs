{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module DBTypes.Consumption where

import Data.Functor.Contravariant ((>$<))
import Data.Monoid ((<>))
import Data.Time.Clock (UTCTime)
import qualified Hasql.Decoders as Decode
import qualified Hasql.Encoders as Encode

import DBTypes (DBTuple(..), KeyedTable, Table(..), WritableTable)
import qualified UnambiguiousStrings as US
import UUIDHelpers (UUID)

data Row = Row {
  consumer :: UUID,
  item :: US.SText,
  happened :: UTCTime
}

instance DBTuple Row where
  columns = const ["consumer", "item", "happened"]
  encoder = const $ (  (consumer >$< (Encode.param Encode.uuid))
                    <> (item >$< (Encode.param Encode.text))
                    <> (happened >$< (Encode.param Encode.timestamptz))
                    )
  decoder = const $ do
                      consumer' <- Decode.column Decode.uuid
                      item' <- Decode.column Decode.text
                      happened' <- Decode.column Decode.timestamptz
                      return Row {
                        consumer = consumer',
                        item = item',
                        happened = happened'
                        }

instance Table Row where
  table =  const "consumption"

instance WritableTable Row where {}


