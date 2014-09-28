module Database.Postgresql.Native.Message.Transport (
  receiveMessage
, receiveSSLResponse
, sendMessage
, sendInitialMessage
, sendSCMCredentials
) where

import Control.Exception (catch, throwIO)
import Data.Attoparsec.ByteString (Parser)
import Data.Word (Word32)
import Control.Monad (when)

import qualified Database.Postgresql.Native.Transport as T
import Database.Postgresql.Native.Transport (Transport)
import Database.Postgresql.Native.Message.Deserialization
import Database.Postgresql.Native.Message.Serialization
import Database.Postgresql.Native.Message

consume :: Transport -> Word32 -> Parser a -> IO a
consume t l p = T.consume t l p `catch` tException
    where tException :: T.PacketError -> IO a
          tException (T.ParseError ctx msg) = throwIO $ ParseError ctx msg

receiveMessage :: Transport -> Word32 -> IO FromBackend
receiveMessage t maxPacketLength = do
  (packetType, packetLen) <- consume t headerLength header
  when (packetLen > maxPacketLength) (throwIO PacketTooLarge)
  consume t (packetLen - 4) (deserializer packetType)

receiveSSLResponse :: Transport -> IO SSLResponse
receiveSSLResponse t = consume t sslResponseLength sslResponse

sendMessage :: Transport -> FromFrontend -> IO ()
sendMessage t m = T.send t (serialize m)

sendInitialMessage :: Transport -> InitialMessage -> IO ()
sendInitialMessage t m = T.send t (serializeInitial m)

sendSCMCredentials :: Transport -> IO ()
sendSCMCredentials = T.sendSCMCredentials
