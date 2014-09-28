{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Main where

import Control.Exception
import Control.Monad (when)
import Data.Typeable

import qualified Database.Postgresql.Native.Transport as T
import qualified Database.Postgresql.Native.Connection as C
import qualified Database.Postgresql.Native.Connection.OpenSSL as CO
import qualified Database.Postgresql.Native.Connection.Connection as CC
import qualified Network.Connection as NC
import Database.Postgresql.Native.Connection.Tracing
import Database.Postgresql.Native.Message
import Database.Postgresql.Native.Message.Transport
import Database.Postgresql.Native.Types
import Data.Default.Class (def)

import Database.Postgresql.Native.Utils

import OpenSSL (withOpenSSL)
import qualified OpenSSL.Session as OSSL

data ConnectionFailure = SSLNotSupportedByServer
                       | BadCredsType
                       | UnsupportedAuthenticationRequirement
                       | ConnectionRejected [(MessageField, ByteString0)]
                         deriving (Show, Typeable)
instance Exception ConnectionFailure

initOSSLCtx :: IO OSSL.SSLContext
initOSSLCtx = do
  sslCtx <- OSSL.context
  OSSL.contextSetCiphers sslCtx "DEFAULT"
  OSSL.contextSetVerificationMode sslCtx OSSL.VerifyNone
  return sslCtx

initNCCtx :: IO NC.ConnectionContext
initNCCtx = NC.initConnectionContext

createOSSLConnection :: OSSL.SSLContext -> IO C.Connection
createOSSLConnection ctx = CO.connect ctx "localhost" 5432

createNCConnection :: NC.ConnectionContext -> IO C.Connection
createNCConnection ctx = CC.connect ctx NC.ConnectionParams {
                           NC.connectionHostname = "localhost"
                         , NC.connectionPort = 5432
                         , NC.connectionUseSecure =
                             Just def { NC.settingDisableCertificateValidation = True }
                         , NC.connectionUseSocks = Nothing
                         }

main :: IO ()
main = withOpenSSL $ bracket (initOSSLCtx >>= conn) T.closeRudely go
    where conn ctx = T.open def { T.createConnection = createConnection ctx }
          createConnection ctx = bracketOnError (createOSSLConnection ctx)
                                                C.closeRudely
                                                (trace putStrLn)
          creds = UsernameAndPassword "pgnative" "pgnative"
          go t = do
            maybeUpgrade t
            login creds t
            -- TODO: Set up receive loop, parameter store, backend key
            awaitRFQ t
            sendMessage t $ Query "select * from three_rows"
            -- sendMessage t $ Query "LISTEN gnu" -- "select * from three_rows"
            receiveMessagesUntilError t
            T.closeNicely t

awaitRFQ :: T.Transport -> IO ()
awaitRFQ t = do
  msg <- nextMessage t
  case msg of
    ReadyForQuery _ -> return ()
    _ -> awaitRFQ t

receiveMessagesForever :: T.Transport -> IO a
receiveMessagesForever t = do
  _ <- nextMessage t
  receiveMessagesForever t

receiveMessagesUntilError :: T.Transport -> IO ()
receiveMessagesUntilError t = do
  msg <- nextMessage t
  case msg of
    ErrorResponse _ -> return ()
    _ -> receiveMessagesUntilError t

nextMessage :: T.Transport -> IO FromBackend
nextMessage t = do
  msg <- receiveMessage t (1024*1024)
  print msg
  return msg

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
  msg <- nextMessage t
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
