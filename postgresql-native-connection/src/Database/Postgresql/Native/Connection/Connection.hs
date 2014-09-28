module Database.Postgresql.Native.Connection.Connection (
  connect
) where

import Control.Exception (bracketOnError, catch, throwIO)
import System.IO.Error (isEOFError)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Network.Connection as C
import Data.IORef (newIORef, readIORef, writeIORef)
import Control.Monad (unless)

import Database.Postgresql.Native.Connection

connect :: C.ConnectionContext -> C.ConnectionParams -> IO Connection
connect ctx params = bracketOnError doConnect C.connectionClose wrapConnect
    where doConnect = C.connectTo ctx params{C.connectionUseSecure = Nothing}
          wrapConnect conn = do
            closed <- newIORef False
            let checkClosed op = do
                          isClosed <- readIORef closed
                          unless isClosed op
                setClosed = writeIORef closed True
            return Connection {
                               send = mapM_ (C.connectionPut conn) . BSL.toChunks
                             , recv = eofToEmpty $ C.connectionGet conn
                             , closeNicely = (checkClosed $ C.connectionClose conn) >> setClosed
                             , closeRudely = (checkClosed $ C.connectionClose conn) >> setClosed
                             , makeSSL = fmap (sslify ctx conn) (C.connectionUseSecure params)
                             , sendSCMCredentials = Nothing
                             }

sslify :: C.ConnectionContext -> C.Connection -> C.TLSSettings -> IO ()
sslify ctx conn settings = do
  s <- C.connectionIsSecure conn
  if s
  then error "already SSL" -- or should it just ignore?
  else C.connectionSetSecure ctx conn settings

eofToEmpty :: (Int -> IO BS.ByteString) -> Int -> IO BS.ByteString
eofToEmpty op n = op n `catch` handle
    where handle e = if isEOFError e then return BS.empty else throwIO e
