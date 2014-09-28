module Database.Postgresql.Native.Connection (
  Connection(..)
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

-- |A transport for @ByteString@s, potentially upgradable from
-- plaintext to SSL.  A 'Connection' must support sending and
-- receiving from two different threads simultaneously, but need not
-- be thread-safe in any other respect.
data Connection = Connection {
      send :: BSL.ByteString -> IO ()
    -- ^ Sends all the bytes.  The behavior is undefined if called on
    -- a closed 'Connection'.
    , recv :: Int -> IO BS.ByteString
    -- ^ Receives _at most_ the given number of bytes.  Returns an
    -- empty 'BS.ByteString' if the remote end has closed the
    -- connection.  Behavior is undefined if called on a closed
    -- 'Connection'.
    , closeNicely :: IO ()
    -- ^ Closes the connection normally, including gracefully
    -- terminating the connection if it has been upgraded to SSL.  A
    -- 'Connection' may be closed multiple times.
    , closeRudely :: IO ()
    -- ^ Closes the connection abrubtly.  In particular, might not do
    -- an SSL shutdown.  A 'Connection' may be closed multiple times.
    , makeSSL :: Maybe (IO ())
    -- ^ An IO action which will upgrade this connection to SSL, if
    -- supported.  Behavior is undefined if upgraded more than once.
    , sendSCMCredentials :: Maybe (IO ())
    -- ^ An IO action which will send an SCM_CREDS cmsg together with
    -- a single byte if this connection supports that.  Behavior is
    -- undefined if called on a closed 'Connection'.
    }
