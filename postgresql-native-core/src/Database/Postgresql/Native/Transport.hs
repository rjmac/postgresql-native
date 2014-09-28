{-# LANGUAGE CPP, DeriveDataTypeable, RecordWildCards,NamedFieldPuns #-}

module Database.Postgresql.Native.Transport (
  Transport
, TransportSettings(..)
, open
, clear
, recv
, send
, makeSSL
, canMakeSSL
, consume
, closeNicely
, closeRudely
, sendSCMCredentials
, PacketError(..)
, def
) where

import Control.Exception (Exception, bracketOnError, throwIO)
import Control.Applicative ((<$>),(<*))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Monad (when)
import Data.Typeable (Typeable)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString (parse, Result, IResult(..), Parser, endOfInput)
import Data.Word (Word32)
import Data.Maybe (fromMaybe, isJust)
import qualified Database.Postgresql.Native.Connection as C
#ifdef NO_UNIX_DOMAIN_SOCKETS
import qualified Database.Postgresql.Native.Connection.TCPSocket as TCP
#else
import qualified Database.Postgresql.Native.Connection.UnixDomainSocket as UDS
#endif
import Data.Default.Class (Default, def)

data PacketError = ParseError [String] String
                   deriving (Show, Typeable)

instance Exception PacketError

data Transport = Transport { tSettings :: TransportSettings
                           , tConn :: C.Connection
                           , tBufSize :: Int
                           , tRemainingRef :: IORef ByteString }

data TransportSettings = TransportSettings {
      createConnection :: IO C.Connection
    , bufSize :: Int
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
                            }

sendSCMCredentials :: Transport -> IO ()
sendSCMCredentials Transport{tConn} = fromMaybe (error "Cannot send SCM credentials over connection") $ C.sendSCMCredentials tConn

open :: TransportSettings -> IO Transport
open ts@TransportSettings{..} =
    bracketOnError createConnection C.closeRudely new
        where new c = Transport ts c bufSize <$> newIORef BS.empty

available :: Transport -> IO Int
available Transport{tRemainingRef} = BS.length <$> readIORef tRemainingRef

recv :: Transport -> IO Int
recv t@Transport{..} = do
  remaining <- available t
  when (remaining /= 0) $ error "recv called with bytes remaining in the buffer"
  bs <- C.recv tConn tBufSize
  writeIORef tRemainingRef bs
  return $ BS.length bs

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

consume :: Transport -> Word32 -> Parser a -> IO a
consume pb amount parser = do
  let constrainedParser = parse $ parser <* endOfInput
  remaining <- available pb
  if remaining == 0
  then recvAndConsume pb amount constrainedParser
  else doConsume pb amount constrainedParser

recvAndConsume :: Transport -> Word32 -> (ByteString -> Result a) -> IO a
recvAndConsume t amt parser = do
  _ <- recv t
  doConsume t amt parser

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
                       recvAndConsume t (amount - fromIntegral toRead) cont
      Done i r | BS.null i -> return r
               | otherwise -> error "non-empty leftovers?"
