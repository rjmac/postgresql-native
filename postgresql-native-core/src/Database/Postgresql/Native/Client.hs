{-# LANGUAGE NamedFieldPuns, LambdaCase, RecordWildCards, DeriveDataTypeable, OverloadedStrings #-}

module Database.Postgresql.Native.Client (
  connect
, connectEx
, Client
, ClientSettings(..)
, OptionalClientSettings(..)
, closeNicely
, closeRudely
, def
-- temporary methods until an actual API is set up
, sendMessage
, nextMessage
-- Start of the actual API
, parameter
, cancel
) where

import Control.Exception (Exception, bracketOnError, onException, mask_, throwIO)
import Control.Monad (when)
import Data.IORef (IORef, newIORef, writeIORef, readIORef, modifyIORef)
import Data.Default.Class (Default, def)
import Data.Typeable
import qualified Data.Map.Strict as Map

import Database.Postgresql.Native.Authenticator.Internal (Authenticator(..))
import Database.Postgresql.Native.Connection (Connection)
import qualified Database.Postgresql.Native.Connection as C
import Database.Postgresql.Native.Transport (Transport)
import qualified Database.Postgresql.Native.Transport as T
import Database.Postgresql.Native.Message
import Database.Postgresql.Native.ProtocolError (ProtocolError(UnexpectedMessage))
import Database.Postgresql.Native.Types

data State = Closed
           -- ^ The 'Client' has been closed and can no longer be
           -- used.
           | Broken
           -- ^ A fatal error has been received, but the 'Client' is
           -- not yet actually closed.
           | Idle
           -- ^ The connection is healthy and the 'Client' has no
           -- operations in progress.
           | AwaitingResponse
           -- ^ A query has been sent but no response has yet been
           -- received.
           | ReceivingRows
           -- ^ A 'RowDescription' message was received but not yet a
           -- subsequent 'CommandComplete'.
           | CopyIn
           -- ^ A 'CopyinResponse' message has been received but not
           -- yet a subsequent 'CommandComplete'.
           | CopyOut
           -- ^ A 'CopyOutResponse' message has been received but not
           -- yet a subsequent 'CommandComplete'.
           | ResponseReceived
           -- ^ A 'CommandComplete' message has been received but not
           -- yet either 'ReadyForQuery' or more responses.
             deriving (Read, Show, Eq, Ord, Enum, Bounded)

data BackendKey = BackendKey !ServerPid !ServerKey

data Client = Client { clientState :: IORef State
                     , sessionParameters :: IORef (Map.Map ParameterName ByteString0)
                     , backendKey :: !BackendKey
                     , transport :: Transport
                     , clientSettings :: ClientSettings
                     , optionalClientSettings :: OptionalClientSettings
                     }

data ClientSettings =
    ClientSettings { connectionProvider :: IO Connection
                   , username :: ByteString0
                   , initialDatabase :: ByteString0
                   , authenticator :: Authenticator }

data ConnectionFailure = SSLNotSupportedByServer
                       | UnsupportedAuthenticationRequirement AuthResultCode
                       -- ^ The server demanded an authentication
                       -- method that the provided 'Authenticator' is
                       -- not willing to handle.
                       | ConnectionRejected [(MessageField, ByteString0)]
                         deriving (Show, Typeable)
instance Exception ConnectionFailure

-- TODO: This needs to go somewhere
-- defaultOpen :: IO C.Connection
-- #ifdef NO_UNIX_DOMAIN_SOCKETS
-- defaultOpen = TCP.connect "localhost" 5432
-- #else
-- defaultOpen = UDS.connect "/var/run/postgresql/.s.PGSQL.5432"
-- #endif

-- Should this just be a Maybe field inside client settings?
data OptionalClientSettings =
    OptionalClientSettings { transportSettings :: T.TransportSettings
                           , applicationName :: ByteString0 }

instance Default OptionalClientSettings where
    def = OptionalClientSettings { transportSettings = def
                                 , applicationName = "" }

connect :: ClientSettings -> IO Client
connect cs = connectEx cs def

connectEx :: ClientSettings -> OptionalClientSettings -> IO Client
connectEx clientSettings@ClientSettings{..} optionalClientSettings@OptionalClientSettings{..} =
    bracketOnError connectionProvider C.closeRudely go
        where go conn = do
                transport <- T.open conn transportSettings
                maybeUpgrade transport
                login transport username initialDatabase applicationName authenticator
                (sessionParametersValue, backendKey) <- receiveSessionData transport
                print sessionParametersValue
                sessionParameters <- newIORef sessionParametersValue
                clientState <- newIORef Idle
                return Client {..}

maybeUpgrade :: T.Transport -> IO ()
maybeUpgrade t =
    when (T.canMakeSSL t) $ do
      T.sendInitialMessage t SSLRequest
      resp <- T.receiveSSLResponse t
      case resp of
        SSLOK -> T.makeSSL t
        SSLNotOK -> throwIO SSLNotSupportedByServer
        SSLFatal -> throwIO SSLNotSupportedByServer

login :: T.Transport -> ByteString0 -> ByteString0 -> ByteString0 -> Authenticator -> IO ()
login t usr initdb appName (Authenticator auth) = do
  T.sendInitialMessage t $ StartupMessage [("user",usr)
                                          ,("database",initdb)
                                          ,("DateStyle","ISO, MDY")
                                          ,("client_encoding","UTF8")
                                          ,("application_name",appName)]
  T.nextMessage t >>= \case
    AuthenticationResponse code ->
        case auth code of
          Just op -> op t usr
          Nothing -> throwIO $ UnsupportedAuthenticationRequirement code
    ErrorResponse c -> error $ show c -- TODO: I need errors!
    other -> throwIO $ UnexpectedMessage other

receiveSessionData :: T.Transport -> IO (Map.Map ParameterName ByteString0, BackendKey)
receiveSessionData t = go Map.empty Nothing
    where go sessionParameters backendKey = T.nextMessage t >>= \case
            ParameterStatus param value -> go (Map.insert param value sessionParameters) backendKey
            BackendKeyData p k -> go sessionParameters (Just $ BackendKey p k)
            ReadyForQuery _ -> return (sessionParameters, maybe (error "TODO no backend key") id backendKey)
            ErrorResponse c -> error $ show c -- TODO
            NoticeResponse c -> error $ show c -- TODO
            other -> throwIO $ UnexpectedMessage other

withMessageDrain :: Client -> IO a -> IO a
withMessageDrain c a = a -- TODO: actually drain messages

unlessClosedAlready :: Client -> IO () -> IO ()
unlessClosedAlready client action = ifStateIs client Closed (return ()) action

ifStateIs :: Client -> State -> IO a -> IO a -> IO a
ifStateIs Client{clientState} state onThen onElse = do
  s <- readIORef clientState
  if s == state then onThen else onElse

-- | Sends a 'Terminate' message to the server and then waits for the
-- server to close the connection.  If an exception is received,
-- delegates to 'closeRudely' (i.e., once this is called the client
-- /will/ be closed).
closeNicely :: Client -> IO ()
closeNicely client = mask_ (terminateAndAwaitClose `onException` closeRudely client)
    where terminateAndAwaitClose = unlessClosedAlready client $ do
            withMessageDrain client $ T.sendMessage (transport client) Terminate
            awaitClose
            T.closeNicely $ transport client
            writeIORef (clientState client) Closed
          awaitClose = T.receiveMessage (transport client) >>= maybe (return ()) (const awaitClose)

-- | Simply drops the connection to the server without being graceful about it.
closeRudely :: Client -> IO ()
closeRudely client = mask_ $ unlessClosedAlready client $ do
  writeIORef (clientState client) Closed
  T.closeRudely $ transport client

sendMessage :: Client -> FromFrontend -> IO ()
sendMessage Client{transport} msg = T.sendMessage transport msg

nextMessage :: Client -> IO FromBackend
nextMessage Client{transport, sessionParameters} = do
  msg <- T.nextMessage transport
  case msg of
    ParameterStatus p v -> modifyIORef sessionParameters $! Map.insert p v -- TODO: this might imply changing charsets, etc.
    _ -> return ()
  return msg

parameter :: Client -> ParameterName -> IO (Maybe ByteString0)
parameter Client{sessionParameters} pn = do
  currentParams <- readIORef sessionParameters
  return $ Map.lookup pn currentParams

-- | Cancels an in-progress operation.  This creates a new connection
-- to the database to perform the cancellation by re-using the 'IO'
-- 'Connection' action originally provided in 'ClientSettings'.  In
-- order to work correctly, that action must reconnect to the same
-- server.
cancel :: Client -> IO ()
cancel Client{ backendKey = BackendKey serverPid serverKey
             , clientSettings = ClientSettings{connectionProvider}
             , optionalClientSettings = OptionalClientSettings{transportSettings} } =
    bracketOnError connectionProvider C.closeRudely go
        where go conn = do
                t <- T.open conn transportSettings
                maybeUpgrade t
                T.sendInitialMessage t $ CancelRequest serverPid serverKey
                T.receiveMessage t >>= maybe (return ()) (throwIO . UnexpectedMessage)
                T.closeNicely t
