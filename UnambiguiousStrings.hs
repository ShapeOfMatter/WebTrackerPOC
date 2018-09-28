module UnambiguiousStrings where

import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as SText
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import qualified Data.Text.Lazy as LText

type SBytes = SBS.ByteString
type LBytes = LBS.ByteString
type SText = SText.Text
type LText = LText.Text

packLText = LText.pack
packSText = SText.pack
packLBytes = LBS.pack
packSBytes = SBS.pack

unpackLText = LText.unpack
unpackSText = SText.unpack
unpackLBytes = LBS.unpack
unpackSBytes = SBS.unpack

fromStrictBytes = LBS.fromStrict
toStrictBytes = LBS.toStrict
fromStrictText = LText.fromStrict
toStrictText = LText.toStrict

strictDecodeEither = decodeUtf8'
lazyDecodeEither = (fmap fromStrictText) . decodeUtf8' . toStrictBytes
strictEncode = encodeUtf8
lazyEncode = fromStrictBytes . encodeUtf8 . toStrictText


{-
stBytesFromLzBytes = LBS.toStrict
stBytesFromStText = encodeUtf8
stBytesFromLzText = encodeUtf8 . LText.toStrict
stBytesFromString = encodeUtf8 . SText.pack

lzBytesFromStBytes = LBS.fromStrict
lzBytesFromStText = LBS.fromStrict . encodeUtf8
lzBytesFromLzText = LBS.fromStrict . encodeUtf8 . LText.toStrict
lzBytesFromString = LBS.fromStrict . encodeUtf8 . SText.pack

eStTextFromStBytes = decodeUtf8'
eStTextFromLzBytes = decodeUtf8' . LBS.toStrict
stTextFromLzText = LText.toStrict
stTextFromString = encodeUtf8 . SText.pack

eLzTextFromStBytes = LBS.fromStrict
eLzTextFromLzBytes = LBS.fromStrict . encodeUtf8
lzTextFromStText = LBS.fromStrict . encodeUtf8 . LText.toStrict
lzTextFromString = LBS.fromStrict . encodeUtf8 . SText.pack

eStringFromStBytes
eStringFromLzBytes = LBS.toStrict
stringFromStText = encodeUtf8
stringFromLzText = encodeUtf8 . LText.toStrict
-}


