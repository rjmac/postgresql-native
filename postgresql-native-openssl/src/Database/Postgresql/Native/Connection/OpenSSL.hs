module Database.Postgresql.Native.Connection.OpenSSL (
  connect
) where

import Control.Exception (bracketOnError, catch, SomeException, throwIO, finally)
import Control.Monad (join)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified OpenSSL.Session as OSSL
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import Database.Postgresql.Native.Connection

defaultAddrInfo :: NS.AddrInfo
defaultAddrInfo = NS.defaultHints {
                    NS.addrFlags = [NS.AI_NUMERICSERV, NS.AI_ADDRCONFIG]
                  , NS.addrSocketType = NS.Stream
                  }

connect :: OSSL.SSLContext -> NS.HostName -> NS.PortNumber -> IO Connection
connect sslctx host port = do
  addrs <- NS.getAddrInfo (Just defaultAddrInfo) (Just host) (Just $ show port)
  firstSuccessful $ map (tryConnect sslctx) addrs

data Transport = Socket NS.Socket
               | SSL NS.Socket OSSL.SSL

transport :: (NS.Socket -> a) -> (OSSL.SSL -> a) -> IORef Transport -> IO a
transport viaSocket viaSSL = transport' viaSocket (const viaSSL)

transport' :: (NS.Socket -> a) -> (NS.Socket -> OSSL.SSL -> a) -> IORef Transport -> IO a
transport' viaSocket viaSSL tRef = do
  t <- readIORef tRef
  case t of
    Socket s -> return $ viaSocket s
    SSL sock ssl -> return $ viaSSL sock ssl

tryConnect :: OSSL.SSLContext -> NS.AddrInfo -> IO Connection
tryConnect sslctx ai = bracketOnError createSocket NS.close doConnect
    where createSocket = NS.socket (NS.addrFamily ai) (NS.addrSocketType ai) (NS.addrProtocol ai)
          doConnect s = do
            NS.connect s (NS.addrAddress ai)
            ref <- newIORef $ Socket s
            return $ Connection {
                          send = sendVia ref
                        , recv = recvVia ref
                        , closeNicely = closeNicelyVia ref
                        , closeRudely = closeRudelyVia ref
                        , makeSSL = Just (sslify sslctx ref)
                        , sendSCMCredentials = Nothing
                        }

sendVia :: IORef Transport -> BSL.ByteString -> IO ()
sendVia t bs = transport toSocket OSSL.lazyWrite t >>= ($ bs)
    where toSocket s = NSB.sendMany s . BSL.toChunks

recvVia :: IORef Transport -> Int -> IO BS.ByteString
recvVia t n = transport NSB.recv OSSL.read t >>= ($ n)

closeNicelyVia :: IORef Transport -> IO ()
closeNicelyVia = join . transport' NS.close closessl
    where closessl :: NS.Socket -> OSSL.SSL -> IO ()
          closessl sock ssl = OSSL.shutdown ssl OSSL.Bidirectional `finally` NS.close sock

closeRudelyVia :: IORef Transport -> IO ()
closeRudelyVia = join . transport' NS.close (flip $ const NS.close)

sslify :: OSSL.SSLContext -> IORef Transport -> IO ()
sslify sslctx ref = do
  current <- readIORef ref
  case current of
    Socket s -> do
      ssl <- OSSL.connection sslctx s
      OSSL.connect ssl
      writeIORef ref (SSL s ssl)
    SSL _ _ -> error "already SSL" -- or should it just ignore?

firstSuccessful :: [IO a] -> IO a
firstSuccessful [] = error "firstSuccessful: empty list"
firstSuccessful (p:ps) = p `catch` \e ->
    case ps of
        [] -> throwIO (e :: SomeException)
        _  -> firstSuccessful ps
