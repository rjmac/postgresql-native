{-# LANGUAGE CPP, RecordWildCards,NamedFieldPuns #-}

module Database.Postgresql.Native.Transport (
  Transport
, TransportSettings(..)
, def
, open
, closeNicely
, closeRudely
, canMakeSSL
, makeSSL
, receiveMessage
, receiveSSLResponse
, sendMessage
, sendInitialMessage
, sendSCMCredentials
) where

import Control.Exception (bracketOnError, throwIO)
import Control.Applicative ((<$>),(<*))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Monad (when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString (parse, Result, IResult(..), Parser, endOfInput)
import Data.Word (Word32)
import Data.Maybe (fromMaybe, isJust)

import Database.Postgresql.Native.ProtocolError (ProtocolError(PacketTooLarge, ParseError, UnexpectedEndOfInput))
import Database.Postgresql.Native.Message (FromBackend, FromFrontend, InitialMessage, SSLResponse)
import Database.Postgresql.Native.Message.Serialization (serialize, serializeInitial)
import Database.Postgresql.Native.Message.Deserialization
import qualified Database.Postgresql.Native.Connection as C
#ifdef NO_UNIX_DOMAIN_SOCKETS
import qualified Database.Postgresql.Native.Connection.TCPSocket as TCP
#else
import qualified Database.Postgresql.Native.Connection.UnixDomainSocket as UDS
#endif
import Data.Default.Class (Default, def)

data Transport = Transport { tSettings :: TransportSettings
                           , tConn :: C.Connection
                           , tRemainingRef :: IORef ByteString }

data TransportSettings = TransportSettings {
      createConnection :: IO C.Connection
    , bufSize :: Int
    , maxPacketSize :: Word32
    , trace :: String -> IO ()
    }

defaultOpen :: IO C.Connection
#ifdef NO_UNIX_DOMAIN_SOCKETS
defaultOpen = TCP.connect "localhost" 5432
#else
defaultOpen = UDS.connect "/var/run/postgresql/.s.PGSQL.5432"
#endif

instance Default TransportSettings where
    def = TransportSettings { createConnection = defaultOpen
                            , bufSize = 4096
                            , maxPacketSize = 1024*1024
                            , trace = const $ return ()
                            }

sendSCMCredentials :: Transport -> IO ()
sendSCMCredentials Transport{tConn} = fromMaybe (error "Cannot send SCM credentials over connection") $ C.sendSCMCredentials tConn

open :: TransportSettings -> IO Transport
open ts@TransportSettings{..} =
    bracketOnError createConnection C.closeRudely new
        where new c = Transport ts c <$> newIORef BS.empty

available :: Transport -> IO Int
available Transport{tRemainingRef} = BS.length <$> readIORef tRemainingRef

recv :: Transport -> IO Int
recv t@Transport{..} = do
  remaining <- available t
  when (remaining /= 0) $ error "recv called with bytes remaining in the buffer"
  bs <- C.recv tConn (bufSize tSettings)
  writeIORef tRemainingRef bs
  return $ BS.length bs

recvSome :: Transport -> IO ()
recvSome t = do
  n <- recv t
  when (n == 0) (throwIO UnexpectedEndOfInput)

send :: Transport -> BSL.ByteString -> IO ()
send Transport{tConn} bs = C.send tConn bs

clear :: Transport -> IO ()
clear Transport { tRemainingRef } = do
  writeIORef tRemainingRef BS.empty

makeSSL :: Transport -> IO ()
makeSSL t@Transport{tConn} = do
  fromMaybe (error "Not an SSL-capable connection") $ C.makeSSL tConn
  clear t

canMakeSSL :: Transport -> Bool
canMakeSSL Transport{tConn} = isJust $ C.makeSSL tConn

closeNicely :: Transport -> IO ()
closeNicely t@Transport{tConn} = do
  C.closeNicely tConn
  clear t

closeRudely :: Transport -> IO ()
closeRudely t@Transport{tConn} = do
  C.closeRudely tConn
  clear t

constrainedParser :: Parser a -> (ByteString -> Result a)
constrainedParser parser = parse $ parser <* endOfInput

-- | Consume a message, permitting EOF
consume :: Transport -> Word32 -> Parser a -> IO (Maybe a)
consume t amount parser = do
  remaining <- available t
  if remaining == 0
  then do
    count <- recv t
    if count == 0
    then return Nothing
    else Just <$> doConsume t amount (constrainedParser parser)
  else Just <$> doConsume t amount (constrainedParser parser)

-- | Consume a message, not permitting EOF
consume' :: Transport -> Word32 -> Parser a -> IO a
consume' t amount = doConsume t amount . constrainedParser

doConsume :: Transport -> Word32 -> (ByteString -> Result a) -> IO a
doConsume t@Transport{tRemainingRef} amount parser = do
  buf <- readIORef tRemainingRef
  let toRead = fromIntegral $ min (fromIntegral $ BS.length buf) amount
  writeIORef tRemainingRef $ BS.drop toRead buf
  case parser $ BS.take toRead buf of
      Fail _ ss s ->
          throwIO (ParseError ss s)
      Partial cont | fromIntegral toRead == amount ->
                       case cont BS.empty of
                         Fail _ ss s -> throwIO (ParseError ss s)
                         Partial _ -> error "Partial after EOF?"
                         Done i r | BS.null i -> return r
                                  | otherwise -> error "non-empty leftovers?"
                   | otherwise ->
                       recvSome t >> doConsume t (amount - fromIntegral toRead) cont
      Done i r | BS.null i -> return r
               | otherwise -> error "non-empty leftovers?"

receiveMessage :: Transport -> IO (Maybe FromBackend)
receiveMessage t@Transport{tSettings} = do
  headerInfo <- consume t headerLength header
  case headerInfo of
    Just (packetType, packetLen) ->
        do when (packetLen > maxPacketSize tSettings) (throwIO PacketTooLarge)
           msg <- consume' t (packetLen - 4) (deserializer packetType)
           trace tSettings $ "-->  " ++ show msg
           return $ Just msg
    Nothing ->
        do trace tSettings "-->  [end of input]"
           return Nothing

receiveSSLResponse :: Transport -> IO SSLResponse
receiveSSLResponse t@Transport{tSettings} = do
  msg <- consume' t sslResponseLength sslResponse
  trace tSettings $ "-->  " ++ show msg
  return msg

sendMessage :: Transport -> FromFrontend -> IO ()
sendMessage t@Transport{tSettings} m = do
  trace tSettings $ "<--  " ++ show m
  send t (serialize m)

sendInitialMessage :: Transport -> InitialMessage -> IO ()
sendInitialMessage t@Transport{tSettings} m = do
  trace tSettings $ "<--  " ++ show m
  send t (serializeInitial m)
