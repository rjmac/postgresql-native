module Database.Postgresql.Native.Connection (
  Connection(..)
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

data Connection = Connection {
      send :: BSL.ByteString -> IO ()
    , recv :: Int -> IO BS.ByteString
    , closeNicely :: IO ()
    , closeRudely :: IO ()
    , makeSSL :: Maybe (IO ())
    , sendSCMCredentials :: Maybe (IO ())
    }
