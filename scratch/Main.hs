{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, LambdaCase, BangPatterns #-}

module Main where

import Control.Exception
import Control.Monad (when, forever)
import Data.Typeable
import Data.Monoid (Monoid, mempty, mappend, (<>))
import Control.Concurrent
import Control.Applicative ((<|>))

import Database.Postgresql.Native.Authenticator
import qualified Database.Postgresql.Native.Transport as T
import qualified Database.Postgresql.Native.Connection as C
import qualified Database.Postgresql.Native.Client as Client
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

initOSSLCtx :: IO OSSL.SSLContext
initOSSLCtx = do
  sslCtx <- OSSL.context
  OSSL.contextSetCiphers sslCtx "DEFAULT"
  OSSL.contextSetVerificationMode sslCtx OSSL.VerifyNone
  return sslCtx

initNCCtx :: IO NC.ConnectionContext
initNCCtx = NC.initConnectionContext

createOSSLConnection :: OSSL.SSLContext -> IO C.Connection
createOSSLConnection ctx = CO.connect ctx CO.noopPostUpgradeCheck "localhost" 5432

createNCConnection :: NC.ConnectionContext -> IO C.Connection
createNCConnection ctx = CC.connect ctx NC.ConnectionParams {
                           NC.connectionHostname = "localhost"
                         , NC.connectionPort = 5432
                         , NC.connectionUseSecure =
                             Just def { NC.settingDisableCertificateValidation = True }
                         , NC.connectionUseSocks = Nothing
                         }

main :: IO ()
main = withOpenSSL $ forkAndWait $ bracketOnError (initOSSLCtx >>= open) Client.closeRudely go
    where open ctx =
              Client.connectEx Client.ClientSettings { Client.connectionProvider = createConnection ctx
                                                     , Client.username = "pgnative"
                                                     , Client.initialDatabase = "pgnative_test"
                                                     , Client.authenticator = auth
                                                     }
                               def { Client.transportSettings = def { T.trace = putStrLn } }
          createConnection ctx = bracketOnError (createOSSLConnection ctx)
                                                C.closeRudely
                                                (trace putStrLn)
          auth = defaultAuthenticator <> passwordAuthenticator "pgnative"
          go cli = do
            Client.sendMessage cli $ Query "select * from three_rows"
            awaitRFQ cli

            Client.sendMessage cli $ Query "copy three_rows to stdout csv"
            awaitRFQ cli

            Client.sendMessage cli $ Query "copy three_rows to stdout binary"
            awaitRFQ cli

            Client.sendMessage cli $ Query "LISTEN gnu"
            awaitRFQ cli

            Client.sendMessage cli $ Query "select now()"
            awaitRFQ cli

            Client.sendMessage cli $ Query "SET DateStyle TO 'Postgresql, DMY'"
            awaitRFQ cli

            Client.sendMessage cli $ Query "select now()"
            awaitRFQ cli

            -- let cmd c = (T.sendMessage t $ Query c) >> awaitRFQ t
            -- cmd "begin"
            -- cmd "declare p cursor for select * from cc"
            -- let fetch = do
            --        T.sendMessage t $ Query "fetch forward 50 in p"
            --        let loop !n = nextMessage t >>= \case
            --              DataRow _ -> loop (n+1)
            --              ReadyForQuery _ -> return n
            --              _ -> loop n
            --        loop (0::Int) >>= \case
            --          0 -> return ()
            --          _ -> fetch
            -- fetch
            -- cmd "close p"
            -- cmd "rollback"

            Client.closeNicely cli

awaitRFQ :: Client.Client -> IO ()
awaitRFQ cli =
  Client.nextMessage cli >>= \case
    ReadyForQuery _ -> return ()
    _ -> awaitRFQ cli

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

forkAndWait :: IO t -> IO t
forkAndWait op = do
  v <- newEmptyMVar :: IO (MVar (Either SomeException t))
  _ <- mask_ $ forkIOWithUnmask $ \u -> do
                   (try $ u op) >>= putMVar v
  takeMVar v >>= either throwIO return
  
