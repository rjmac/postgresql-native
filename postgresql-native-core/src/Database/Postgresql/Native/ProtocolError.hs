{-# LANGUAGE DeriveDataTypeable #-}

module Database.Postgresql.Native.ProtocolError (
  ProtocolError(..)
) where

import Control.Exception (Exception)
import Data.Typeable (Typeable)

import Database.Postgresql.Native.Message (FromBackend(..))

-- |A @ProtocolError@ is something that should not happen in normal
-- use.  They are unrecoverable.
data ProtocolError = PacketTooLarge
                   -- ^ A message longer than the library is willing
                   -- to commit to holding in memory was received.
                   | ParseError String
                   -- ^ Failed to parse a message object from a
                   -- received packet.  The `String` is an attoparsec
                   -- error.
                   | UnexpectedMessage FromBackend
                   -- ^ A message which was supposed to be impossible
                   -- in the current state was received.  For example,
                   -- an 'AuthenticationResponse' after login was
                   -- completed.
                   | UnexpectedEndOfInput
                   -- ^ The server hung up unexpectedly.
                     deriving (Show, Typeable)

instance Exception ProtocolError
