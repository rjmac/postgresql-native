{-# LANGUAGE LambdaCase #-}

module Database.Postgresql.Native.Authenticator.Internal (
  Authenticator(..)
, expectAuthOK
) where

import Data.Monoid (Monoid, mempty, mappend)
import Control.Applicative ((<|>))
import Control.Exception (throwIO)
import Database.Postgresql.Native.Types(AuthResultCode(..), ByteString0)
import Database.Postgresql.Native.Message(FromBackend(..))
import Database.Postgresql.Native.Transport(Transport, nextMessage)
import Database.Postgresql.Native.ProtocolError(ProtocolError(UnexpectedMessage))

newtype Authenticator = Authenticator (AuthResultCode -> Maybe (Transport -> ByteString0 -> IO ()))

instance Monoid Authenticator where
    mempty = Authenticator $ const Nothing
    mappend (Authenticator a) (Authenticator b) =
        Authenticator $ \ac -> a ac <|> b ac

expectAuthOK :: Transport -> IO ()
expectAuthOK t =
  nextMessage t >>= \case
    AuthenticationResponse AuthOK -> return ()
    ErrorResponse oops -> error $ show oops -- TODO something better!
    other -> throwIO $ UnexpectedMessage other
