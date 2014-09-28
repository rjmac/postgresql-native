module Database.Postgresql.Native.Connection.Connection (
  connect
) where

import Control.Exception (bracketOnError)
import qualified Data.ByteString.Lazy as BSL
import qualified Network.Connection as C

import Database.Postgresql.Native.Connection

connect :: C.ConnectionContext -> C.ConnectionParams -> IO Connection
connect ctx params = bracketOnError doConnect C.connectionClose wrapConnect
    where doConnect = C.connectTo ctx params{C.connectionUseSecure = Nothing}
          wrapConnect conn = return Connection {
                               send = mapM_ (C.connectionPut conn) . BSL.toChunks
                             , recv = C.connectionGet conn
                             , closeNicely = C.connectionClose conn
                             , closeRudely = C.connectionClose conn
                             , makeSSL = fmap (sslify ctx conn) (C.connectionUseSecure params)
                             , sendSCMCredentials = Nothing
                             }

sslify :: C.ConnectionContext -> C.Connection -> C.TLSSettings -> IO ()
sslify ctx conn settings = do
  s <- C.connectionIsSecure conn
  if s
  then error "already SSL" -- or should it just ignore?
  else C.connectionSetSecure ctx conn settings
