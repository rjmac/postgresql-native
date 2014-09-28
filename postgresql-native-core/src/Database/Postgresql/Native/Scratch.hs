{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Database.Postgresql.Native.Scratch where

import Control.Exception
import Control.Monad (when,forever)
import Data.Typeable

import qualified Database.Postgresql.Native.Transport as T
import qualified Database.Postgresql.Native.Connection as C
import qualified Database.Postgresql.Native.Connection.TCPSocket as CTCP
import qualified Database.Postgresql.Native.Connection.UnixDomainSocket as CUDS
import Database.Postgresql.Native.Connection.Tracing (trace)
import Database.Postgresql.Native.Message
import Database.Postgresql.Native.Message.Transport
import Database.Postgresql.Native.Types
import Data.Default.Class (def)

import Database.Postgresql.Native.Utils

-- connUpgrade = C.TLSSettingsSimple {
--                 C.settingDisableCertificateValidation = True
--               , C.settingDisableSession = False
--               , C.settingUseServerName = False
--               }

-- sockUpgrade socket = do
--   sslCtx <- OSSL.context
--   OSSL.contextSetCiphers sslCtx "DEFAULT"
--   OSSL.contextSetVerificationMode sslCtx OSSL.VerifyNone
--   ssl <- OSSL.connection sslCtx socket
--   OSSL.connect ssl
--   TO.new ssl 4096

data ConnectionFailure = SSLNotSupportedByServer
                       | BadCredsType
                       | UnsupportedAuthenticationRequirement
                       | ConnectionRejected [(MessageField, ByteString0)]
                         deriving (Show, Typeable)
instance Exception ConnectionFailure

test :: IO ()
test = bracket conn disconn go
    where conn = T.open def { T.createConnection = createConnection }
          createConnection = bracketOnError (CTCP.connect "localhost" 5432)
                                            C.closeRudely
                                            (trace putStrLn)
          disconn = T.closeRudely
          creds = UsernameAndPassword "pgnative" "pgnative"
          go t = do
            maybeUpgrade t
            login creds t
            -- TODO: Set up receive loop, parameter store, backend key
            -- TODO: await ready for query
            sendMessage t $ Query "COPY three_rows TO STDOUT CSV" -- "select * from three_rows"
            -- sendMessage t $ Query "LISTEN gnu" -- "select * from three_rows"
            _ <- forever $ do
              packet <- receiveMessage t (1024*1024)
              print packet
            T.closeNicely t

maybeUpgrade :: T.Transport -> IO ()
maybeUpgrade t =
    when (T.canMakeSSL t) $ do
      sendInitialMessage t SSLRequest
      resp <- receiveSSLResponse t
      case resp of
        SSLOK -> T.makeSSL t
        SSLNotOK -> throwIO SSLNotSupportedByServer
        SSLFatal -> throwIO SSLNotSupportedByServer

data Credentials = UsernameOnly ByteString0
                 | UsernameAndPassword ByteString0 ByteString0
                   deriving (Read, Show, Eq)

username :: Credentials -> ByteString0
username (UsernameOnly u) = u
username (UsernameAndPassword u _) = u

badCredsType :: Credentials -> String -> IO a
badCredsType _ _ = throwIO BadCredsType

auth :: Credentials -> T.Transport -> IO ()
auth creds t = do
  msg <- receiveMessage t (1024*1024)
  case msg of
    AuthenticationResponse AuthOK ->
        return ()
    AuthenticationResponse CleartextPassword ->
        case creds of
          UsernameAndPassword _ pwd ->
              do sendMessage t $ PasswordMessage $ pwd
                 auth creds t
          _ ->
              badCredsType creds "UsernameAndPassword"
    AuthenticationResponse (MD5Password salt) ->
        case creds of
          UsernameAndPassword usr pwd ->
              do sendMessage t $ PasswordMessage $ md5password usr pwd salt
                 auth creds t
          _ ->
              badCredsType creds "UsernameAndPassword"
    AuthenticationResponse _ ->
        throwIO UnsupportedAuthenticationRequirement
    ErrorResponse fields ->
        throwIO $ ConnectionRejected fields
    other ->
        throwIO $ UnexpectedMessage other

login :: Credentials -> T.Transport -> IO ()
login creds t = do
  sendInitialMessage t $ StartupMessage [("user","pgnative")
                                        ,("database","pgnative_test")
                                        ,("client_encoding","Latin1")]
  auth creds t
