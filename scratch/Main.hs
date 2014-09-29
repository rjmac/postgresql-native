{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, LambdaCase #-}

module Main where

import Control.Exception
import Control.Monad (when, forever)
import Data.Typeable
import Data.Monoid (Monoid, mempty, mappend, (<>))
import Control.Applicative ((<|>))

import qualified Database.Postgresql.Native.Transport as T
import qualified Database.Postgresql.Native.Connection as C
import qualified Database.Postgresql.Native.Connection.OpenSSL as CO
import qualified Database.Postgresql.Native.Connection.Connection as CC
import qualified Network.Connection as NC
import Database.Postgresql.Native.Connection.Tracing
import Database.Postgresql.Native.Message
import Database.Postgresql.Native.Types
import Database.Postgresql.Native.ProtocolError (ProtocolError(UnexpectedEndOfInput))
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

newtype Authenticator = Authenticator (AuthResultCode -> Maybe (T.Transport -> ByteString0 -> IO ()))

instance Monoid Authenticator where
    mempty = Authenticator $ const Nothing
    mappend (Authenticator a) (Authenticator b) =
        Authenticator $ \ac -> a ac <|> b ac

expectAuthOK :: T.Transport -> IO ()
expectAuthOK t =
  nextMessage t >>= \case
    AuthenticationResponse AuthOK -> return ()
    ErrorResponse oops -> error $ show oops
    other -> error $ "unexpected message" ++ show other
                    
defaultAuthenticator :: Authenticator
defaultAuthenticator = Authenticator auth
    where auth AuthOK = Just $ \_ _ -> return ()
          auth SCMCredential = Just $ \t _ -> do
                                 T.sendSCMCredentials t
                                 expectAuthOK t
          auth _ = Nothing

passwordAuthenticator :: ByteString0 -> Authenticator
passwordAuthenticator password = Authenticator auth
    where auth CleartextPassword = Just $ \t _ -> do
                                     T.sendMessage t $ PasswordMessage $ password
                                     expectAuthOK t
          auth (MD5Password salt) = Just $ \t username -> do
                                      T.sendMessage t $ PasswordMessage $ md5password username password salt
                                      expectAuthOK t
          auth _ = Nothing

main :: IO ()
main = withOpenSSL $ bracket (initOSSLCtx >>= conn) T.closeRudely go
    where conn ctx = T.open def { T.createConnection = createConnection ctx
                                , T.trace = putStrLn }
          createConnection ctx = bracketOnError (createOSSLConnection ctx)
                                                C.closeRudely
                                                (trace putStrLn)
          auth = defaultAuthenticator <> passwordAuthenticator "pgnative"
          go t = do
            maybeUpgrade t
            login t "pgnative" auth
            -- TODO: Set up receive loop, parameter store, backend key
            awaitRFQ t
            T.sendMessage t $ Query "select * from three_rows"
            -- T.sendMessage t $ Query "LISTEN gnu" -- "select * from three_rows"
            receiveMessagesUntilError t
            T.sendMessage t Terminate
            T.closeNicely t

awaitRFQ :: T.Transport -> IO ()
awaitRFQ t =
  nextMessage t >>= \case
    ReadyForQuery _ -> return ()
    _ -> awaitRFQ t

receiveMessagesForever :: T.Transport -> IO a
receiveMessagesForever t = forever $ nextMessage t

receiveMessagesUntilError :: T.Transport -> IO ()
receiveMessagesUntilError t =
  nextMessage t >>= \case
    ErrorResponse _ -> return ()
    _ -> receiveMessagesUntilError t

nextMessage :: T.Transport -> IO FromBackend
nextMessage t =
  T.receiveMessage t >>= \case
    Just m -> return m
    Nothing -> throwIO UnexpectedEndOfInput

maybeUpgrade :: T.Transport -> IO ()
maybeUpgrade t =
    when (T.canMakeSSL t) $ do
      T.sendInitialMessage t SSLRequest
      resp <- T.receiveSSLResponse t
      case resp of
        SSLOK -> T.makeSSL t
        SSLNotOK -> throwIO SSLNotSupportedByServer
        SSLFatal -> throwIO SSLNotSupportedByServer

login :: T.Transport -> ByteString0 -> Authenticator -> IO ()
login t username (Authenticator auth) = do
  T.sendInitialMessage t $ StartupMessage [("user",username)
                                          ,("database","pgnative_test")]
  nextMessage t >>= \case
    AuthenticationResponse code ->
        case auth code of
          Just op -> op t username
          Nothing -> error $ "Cannot respond to " ++ show code
    ErrorResponse c -> error $ show c
    other -> error $ "unexpected message: " ++ show other
