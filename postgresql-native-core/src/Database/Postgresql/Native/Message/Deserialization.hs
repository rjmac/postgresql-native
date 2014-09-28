{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module Database.Postgresql.Native.Message.Deserialization (
  header
, headerLength
, deserializer
, sslResponse
, sslResponseLength
) where

import Data.Attoparsec.ByteString as AP
import qualified Data.Attoparsec.ByteString.Char8 as C8
import Data.Vector (Vector, generate, unsafeIndex)
import Data.Attoparsec.Binary
import Data.Word (Word32, Word8)
import Data.Char (chr)
import Control.Applicative

import Database.Postgresql.Native.Message (FromBackend(..), SSLResponse(..))
import Database.Postgresql.Native.Types
import Database.Postgresql.Native.Types.Deserialization

header :: Parser (Word8, Word32)
header = (,) <$> anyWord8 <*> anyWord32be <?> "packet header"

headerLength :: Word32
headerLength = 5 -- 1 byte packet type, 4 bytes packet length

sslResponse :: Parser SSLResponse
sslResponse = sslrespify <$> C8.satisfy sr <?> "sslResponse"
    where sr c = c == 'S' || c == 'N' || c == 'E'
          sslrespify 'S' = SSLOK
          sslrespify 'N' = SSLNotOK
          sslrespify 'E' = SSLFatal -- will only come from ancient postgres servers
          sslrespify _ = error "sslResponse"

sslResponseLength :: Word32
sslResponseLength = 1

deserializer :: Word8 -> Parser FromBackend
deserializer = (deserializerTable `unsafeIndex`) . fromIntegral -- deserializerTable has all 256 possibilities, therefore safe.

deserializerTable :: Vector (Parser FromBackend)
deserializerTable = generate 256 $ baseDeserializer . chr

baseDeserializer :: Char -> Parser FromBackend
baseDeserializer '2' = parseBindComplete
baseDeserializer '3' = parseCloseComplete
baseDeserializer 'A' = parseNotificationResponse
baseDeserializer 'C' = parseCommandComplete
baseDeserializer 'D' = parseDataRow
baseDeserializer 'E' = parseErrorResponse -- TODO: a VERY early error might not have a packet length at all!
baseDeserializer 'G' = parseCopyInResponse
baseDeserializer 'H' = parseCopyOutResponse
baseDeserializer 'I' = parseEmptyQueryResponse
baseDeserializer 'K' = parseBackendKeyData
baseDeserializer 'N' = parseNoticeResponse
baseDeserializer 'R' = parseAuthResponse
baseDeserializer 'S' = parseParameterStatus
baseDeserializer 'T' = parseRowDescription
baseDeserializer 'V' = parseFunctionCallResponse
baseDeserializer 'W' = parseCopyBothResponse
baseDeserializer 'Z' = parseReadyForQuery
baseDeserializer 'c' = parseCopyOutDone
baseDeserializer 'd' = parseCopyDataOut
baseDeserializer 'n' = parseNoData
baseDeserializer 'p' = parseComplete
baseDeserializer 's' = parsePortalSuspended
baseDeserializer 't' = parseParameterDescription
baseDeserializer other = fail $ "Unknown packet type " ++ show other

parseAuthResponse :: Parser FromBackend
parseAuthResponse = AuthenticationResponse <$> (anyWord32be >>= decodeTyp) <?> "authentication response"
    where decodeTyp 0 = return AuthOK
          decodeTyp 2 = return KerberosV5
          decodeTyp 3 = return CleartextPassword
          decodeTyp 5 = MD5Password <$> AP.take 4
          decodeTyp 6 = return SCMCredential
          decodeTyp 7 = return GSS
          decodeTyp 9 = return SSPI
          decodeTyp 8 = GSSContinue <$> takeByteString
          decodeTyp other = fail $ "unknown auth response type " ++ show other

parseParameterStatus :: Parser FromBackend
parseParameterStatus = ParameterStatus <$> parameterName <*> bytestring0 <?> "parameter status"

parseBackendKeyData :: Parser FromBackend
parseBackendKeyData = BackendKeyData <$> serverPid <*> (ServerKey <$> anyWord32be) <?> "backend key data"

parseReadyForQuery :: Parser FromBackend
parseReadyForQuery = ReadyForQuery <$> statusIndicator <?> "ready for query"

parseErrorResponse :: Parser FromBackend
parseErrorResponse = ErrorResponse <$> many1 ((,) <$> messageField <*> bytestring0) <* word8 0 <?> "error response"

parseBindComplete :: Parser FromBackend
parseBindComplete = pure BindComplete <?> "bind complete"

parseCloseComplete :: Parser FromBackend
parseCloseComplete = pure CloseComplete <?> "close complete"

parseCommandComplete :: Parser FromBackend
parseCommandComplete = CommandComplete <$> bytestring0 <?> "command complete"

parseCopyDataOut :: Parser FromBackend
parseCopyDataOut = CopyDataOut <$> takeByteString <?> "copy data"

parseCopyOutDone :: Parser FromBackend
parseCopyOutDone = pure CopyOutDone <?> "copy done"

parseCopyInResponse :: Parser FromBackend
parseCopyInResponse = CopyInResponse <$> formatCode8 <*> listOf formatCode16 <?> "copy in response"

parseCopyOutResponse :: Parser FromBackend
parseCopyOutResponse = CopyOutResponse <$> formatCode8 <*> listOf formatCode16 <?> "copy out response"

parseCopyBothResponse :: Parser FromBackend
parseCopyBothResponse = CopyBothResponse <$> formatCode8 <*> listOf formatCode16 <?> "copy both response"

parseDataRow :: Parser FromBackend
parseDataRow = DataRow <$> listOf col <?> "data row"
    where col = do
            i <- anyWord32be
            if i == -1
            then return Nothing
            else Just <$> AP.take (fromIntegral i)

parseEmptyQueryResponse :: Parser FromBackend
parseEmptyQueryResponse = pure EmptyQueryResponse <?> "empty query response"

parseFunctionCallResponse :: Parser FromBackend
parseFunctionCallResponse = FunctionCallResponse <$> res <?> "function call response"
    where res = do
            n <- anyWord32be
            if n == -1
            then return Nothing
            else Just <$> AP.take (fromIntegral n)

parseNoData :: Parser FromBackend
parseNoData = pure NoData <?> "no data"

parseNoticeResponse :: Parser FromBackend
parseNoticeResponse = NoticeResponse <$> many1 ((,) <$> messageField <*> bytestring0) <* word8 0 <?> "notice response"

parseNotificationResponse :: Parser FromBackend
parseNotificationResponse = NotificationResponse <$> serverPid <*> channelName <*> bytestring0 <?> "notification response"

parseParameterDescription :: Parser FromBackend
parseParameterDescription = ParameterDescription <$> listOf oid <?> "parameter description"

parseComplete :: Parser FromBackend
parseComplete = pure ParseComplete <?> "parse complete"

parsePortalSuspended :: Parser FromBackend
parsePortalSuspended = pure PortalSuspended <?> "portal suspended"

parseRowDescription :: Parser FromBackend
parseRowDescription = RowDescription <$> listOf colDesc <?> "row description"
    where colDesc = RowColumnDescription <$> bytestring0 <*> oid <*> anyWord16be <*> oid <*> (fromIntegral <$> anyWord16be) <*> anyWord32be <*> formatCode16

-- lists of items preceded by a 16-bit count occurs several places in the protocol
listOf :: Parser a -> Parser [a]
listOf p = do
  n <- anyWord16be
  count (fromIntegral n) p
