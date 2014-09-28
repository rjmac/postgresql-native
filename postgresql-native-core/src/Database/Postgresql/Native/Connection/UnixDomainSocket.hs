{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Database.Postgresql.Native.Connection.UnixDomainSocket (
#ifndef NO_UNIX_DOMAIN_SOCKETS
  connect
#endif
) where

#ifndef NO_UNIX_DOMAIN_SOCKETS

import Control.Exception (bracketOnError)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Foreign.C.Error (throwErrnoIfMinus1)
import Foreign.C.Types (CInt(..))

import Database.Postgresql.Native.Connection

connect :: FilePath -> IO Connection
connect path = bracketOnError createSocket NS.close doConnect
    where createSocket = NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol
          doConnect s = do
            NS.connect s $ NS.SockAddrUnix path
            return $ Connection {
                          send = NSB.sendMany s . BSL.toChunks
                        , recv = NSB.recv s
                        , closeNicely = NS.close s
                        , closeRudely = NS.close s
                        , makeSSL = Nothing
                        , sendSCMCredentials = Just $ sendCreds s
                        }

sendCreds :: NS.Socket -> IO ()
sendCreds s = do
  i <- throwErrnoIfMinus1 "sendCreds" $ c_sendCreds $ NS.fdSocket s
  if i == 0
  then error "Cannot sendcreds on this platform"
  else NSB.sendAll s $ BS.singleton 0

c_sendCreds :: CInt -> IO CInt
c_sendCreds _ = return 0

-- TODO: this.  NOT A FAN OF CABAL BTW
-- -- returns positive for success, 0 for "don't have CMSG", -1 for other error
-- foreign import ccall unsafe "hspgnative_sendcreds" c_sendCreds :: CInt -> IO CInt

#endif
