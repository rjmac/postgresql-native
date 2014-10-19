{-# LANGUAGE CPP, RecordWildCards, NamedFieldPuns, LambdaCase #-}

module Database.Postgresql.Native.Transport (
  Transport
, TransportSettings(..)
, def
, open
, closeNicely
, closeRudely
, canMakeSSL
, makeSSL
, nextMessage
, receiveMessage
, receiveSSLResponse
, sendMessage
, sendInitialMessage
, sendSCMCredentials
) where

import Control.Exception (throwIO)
import Control.Applicative ((<$>),(<*))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Monad (when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString (parse, parseOnly, Result, IResult(..), Parser, endOfInput)
import Data.Word (Word32)
import Data.Maybe (fromMaybe, isJust)

import Database.Postgresql.Native.ProtocolError (ProtocolError(PacketTooLarge, ParseError, UnexpectedEndOfInput))
import Database.Postgresql.Native.Message (FromBackend, FromFrontend, InitialMessage, SSLResponse)
import Database.Postgresql.Native.Message.Serialization (serialize, serializeInitial)
import Database.Postgresql.Native.Message.Deserialization
import qualified Database.Postgresql.Native.Connection as C
import Database.Postgresql.Native.Connection (Connection)
import Data.Default.Class (Default, def)

-- | A message-oriented layer atop a byte-oriented 'Connection'.
data Transport = Transport { tSettings :: TransportSettings
                           , tConn :: Connection
                           , tRemainingRef :: IORef ByteString }

data TransportSettings = TransportSettings {
      bufSize :: Int
    , maxPacketSize :: Word32
    , trace :: String -> IO ()
    }

instance Default TransportSettings where
    def = TransportSettings { bufSize = 4096
                            , maxPacketSize = 1024*1024
                            , trace = const $ return ()
                            }

sendSCMCredentials :: Transport -> IO ()
sendSCMCredentials Transport{tConn} = fromMaybe (error "Cannot send SCM credentials over connection") $ C.sendSCMCredentials tConn

-- | Wraps a 'Connection' in a message-oriented transport layer.  Once
-- done, the 'Connection' itself should no longer be used, with the
-- possible exception of a rude close in the event of an error before
-- some higher level is finished initialization.
open :: Connection -> TransportSettings -> IO Transport
open c ts@TransportSettings{..} = Transport ts c <$> newIORef BS.empty

available :: Transport -> IO Word32
available Transport{tRemainingRef} = (fromIntegral . BS.length) <$> readIORef tRemainingRef

recv :: Transport -> IO Word32
recv t@Transport{..} = do
  remaining <- available t
  when (remaining /= 0) $ error "recv called with bytes remaining in the buffer"
  bs <- C.recv tConn (bufSize tSettings)
  writeIORef tRemainingRef bs
  return $ fromIntegral $ BS.length bs

recvSome :: Transport -> IO ()
recvSome t = do
  n <- recv t
  when (n == 0) (throwIO UnexpectedEndOfInput)

send :: Transport -> BSL.ByteString -> IO ()
send Transport{tConn} bs = C.send tConn bs

clear :: Transport -> IO ()
clear Transport { tRemainingRef } = do
  writeIORef tRemainingRef BS.empty

-- | Secures the underlying 'Connection'; it is an error to call
-- this if 'canMakeSSL' is not 'True'.
makeSSL :: Transport -> IO ()
makeSSL t@Transport{tConn} = do
  fromMaybe (error "Not an SSL-capable connection") $ C.makeSSL tConn
  -- security: forget anything already in our buffer since it may have
  -- been placed there by someone manipulating the unsecured TCP
  -- packet that contained the "SSLOK" message.
  clear t

-- | Test whether the underlying 'Connection' can be upgraded to SSL.
-- Check this before trying to do the upgrade with 'makeSSL'.
canMakeSSL :: Transport -> Bool
canMakeSSL Transport{tConn} = isJust $ C.makeSSL tConn

-- | Closes the underlying 'Connection' nicely.
closeNicely :: Transport -> IO ()
closeNicely t@Transport{tConn} = do
  C.closeNicely tConn
  clear t

-- | Closes the underlying 'Connection' rudely.
closeRudely :: Transport -> IO ()
closeRudely t@Transport{tConn} = do
  C.closeRudely tConn
  clear t

-- | Consume a message, permitting EOF
consume :: Transport -> Parser a -> Word32 -> IO (Maybe a)
consume t parser amount = available t >>= maybeRefill
    where maybeRefill 0 = recv t >>= \case
                            0 -> return Nothing
                            n -> Just <$> haveBytes t parser amount n
          maybeRefill n = Just <$> haveBytes t parser amount n

-- | Consume a message, not permitting EOF
consume' :: Transport -> Parser a -> Word32 -> IO a
consume' t parser bytesWanted = available t >>= maybeRefill
    where maybeRefill 0 = recv t >>= haveBytes t parser bytesWanted
          maybeRefill n = haveBytes t parser bytesWanted n

haveBytes :: Transport -> Parser a -> Word32 -> Word32 -> IO a
haveBytes t parser bytesWanted bytesReady =
    let p = parser <* endOfInput
    in if bytesWanted <= bytesReady
       then doConsumeOnly t (parseOnly p) bytesWanted
       else doConsume t (parse p) bytesWanted

-- This is only called when the desired number of bytes are definitely available.
doConsumeOnly :: Transport -> (ByteString -> Either String a) -> Word32 -> IO a
doConsumeOnly Transport{tRemainingRef} parser bytesWanted = do
  buf <- readIORef tRemainingRef
  let toRead = fromIntegral $ bytesWanted
  writeIORef tRemainingRef $ BS.drop toRead buf
  case parser $ BS.take toRead buf of
    Right a -> return a
    Left err -> throwIO (ParseError err)

doConsume :: Transport -> (ByteString -> Result a) -> Word32 -> IO a
doConsume t@Transport{tRemainingRef} parser bytesStillWanted = do
  buf <- readIORef tRemainingRef
  let bytesAlreadyHave = fromIntegral $ min (fromIntegral $ BS.length buf) bytesStillWanted
  writeIORef tRemainingRef $ BS.drop bytesAlreadyHave buf
  case parser $ BS.take bytesAlreadyHave buf of
      Fail _ _ s ->
          throwIO (ParseError s)
      Partial cont | fromIntegral bytesAlreadyHave == bytesStillWanted ->
                       case cont BS.empty of
                         Fail _ _ s -> throwIO (ParseError s)
                         Partial _ -> error "Partial after EOF?"
                         Done i r | BS.null i -> return r
                                  | otherwise -> error "non-empty leftovers?"
                   | otherwise ->
                       recvSome t >> doConsume t cont (bytesStillWanted - fromIntegral bytesAlreadyHave)
      Done i r | BS.null i -> return r
               | otherwise -> error "non-empty leftovers?"

-- | Await the next message.  If this is interrupted, the connection
-- should be considered broken, as it is possible a message will have
-- been partially consumed.
-- 
-- If an EOF is received /instead of/ the start of a new message,
-- Nothing is returned.  An EOF in the middle of a message always
-- throws 'UnexpectedEndOfInput'.
receiveMessage :: Transport -> IO (Maybe FromBackend)
receiveMessage t@Transport{tSettings} =
  -- It would be possible to write this such that it would be tolerant of
  -- async exceptions _if_ the underlying Connection is.  I'm not sure that
  -- can be guaranteed, and so this will not even make the attempt in order
  -- to prevent subtle bugs.
  consume t header headerLength >>= \case
    Just (packetType, packetLen) ->
        do when (packetLen > maxPacketSize tSettings) (throwIO PacketTooLarge)
           msg <- consume' t (deserializer packetType) (packetLen - 4)
           trace tSettings $ "-->  " ++ show msg
           return $ Just msg
    Nothing ->
        do trace tSettings "-->  [end of input]"
           return Nothing

-- | Like 'receiveMessage' but always throws 'UnexpectedEndOfInput' if
-- the socket is closed.
nextMessage :: Transport -> IO FromBackend
nextMessage t = receiveMessage t >>= maybe (throwIO UnexpectedEndOfInput) return 

receiveSSLResponse :: Transport -> IO SSLResponse
receiveSSLResponse t@Transport{tSettings} = do
  msg <- consume' t sslResponse sslResponseLength
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
