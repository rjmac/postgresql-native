module Database.Postgresql.Native.Connection.TCPSocket (
  connect
) where

-- a very simple connection impl that can't even do SSL.  "Real" connections
-- probably want the postgresql-native-connection or postgresql-native-openssl
-- packages, which use TLS and HsOpenSSL respectively. 

import Control.Exception (bracketOnError, catch, SomeException, throwIO)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified Data.ByteString.Lazy as BSL

import Database.Postgresql.Native.Connection

defaultAddrInfo :: NS.AddrInfo
defaultAddrInfo = NS.defaultHints {
                    NS.addrFlags = [NS.AI_NUMERICSERV, NS.AI_ADDRCONFIG]
                  , NS.addrSocketType = NS.Stream
                  }

connect :: NS.HostName -> NS.PortNumber -> IO Connection
connect host port = do
  addrs <- NS.getAddrInfo (Just defaultAddrInfo) (Just host) (Just $ show port)
  firstSuccessful $ map tryConnect addrs

tryConnect :: NS.AddrInfo -> IO Connection
tryConnect ai = bracketOnError createSocket NS.close doConnect
    where createSocket = NS.socket (NS.addrFamily ai) (NS.addrSocketType ai) (NS.addrProtocol ai)
          doConnect s = do
            NS.connect s (NS.addrAddress ai)
            return $ Connection {
                          send = NSB.sendMany s . BSL.toChunks
                        , recv = NSB.recv s
                        , closeNicely = NS.close s
                        , closeRudely = NS.close s
                        , makeSSL = Nothing
                        , sendSCMCredentials = Nothing
                        }

firstSuccessful :: [IO a] -> IO a
firstSuccessful [] = error "firstSuccessful: empty list"
firstSuccessful (p:ps) = p `catch` \e ->
    case ps of
        [] -> throwIO (e :: SomeException)
        _  -> firstSuccessful ps
