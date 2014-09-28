{-# LANGUAGE DeriveDataTypeable #-}

module Database.Postgresql.Native.Message where

import Control.Exception (Exception)
import Data.ByteString (ByteString)
import Data.Word (Word32)
import Data.Typeable (Typeable)

import Database.Postgresql.Native.Types

-- http://www.postgresql.org/docs/9.4/static/protocol-message-formats.html

data FromBackend = AuthenticationResponse AuthResultCode -- 'R'
                 | BackendKeyData ServerPid ServerKey -- 'K'
                 | BindComplete -- '2'
                 | CloseComplete -- '3'
                 | CommandComplete ByteString0 -- 'C'; TODO: parse the tag
                 | CopyDataOut ByteString -- 'd'
                 | CopyOutDone -- 'c'
                 | CopyInResponse FormatCode [FormatCode] -- 'G'
                 | CopyOutResponse FormatCode [FormatCode] -- 'H'
                 | CopyBothResponse FormatCode [FormatCode] -- 'W'
                 | DataRow [Maybe ByteString] -- 'D'
                 | EmptyQueryResponse -- 'I'
                 | ErrorResponse [(MessageField, ByteString0)] -- 'E'; field codes are in 49.6 of the manual
                 | FunctionCallResponse (Maybe ByteString) -- 'V'
                 | NoData -- 'n'
                 | NoticeResponse [(MessageField, ByteString0)] -- 'N'
                 | NotificationResponse ServerPid ChannelName ByteString0 -- 'A'
                 | ParameterDescription [Oid] -- 't'
                 | ParameterStatus ParameterName ByteString0 -- 'S'
                 | ParseComplete -- 'p'
                 | PortalSuspended -- 's'
                 | ReadyForQuery StatusIndicator -- 'Z'
                 | RowDescription [RowColumnDescription] -- 'T'
                   deriving (Show, Read, Eq, Ord)

-- A pseudomessage sent in response to SSLRequest
data SSLResponse = SSLOK | SSLNotOK | SSLFatal deriving (Eq, Ord, Show, Read)

-- If any of these lists have 2^16 or more elements, bad things will happen.
-- On the wire, they are preceded by their length in 16-bit form.
data FromFrontend = Bind PortalName PreparedStatementName BoundParams ResultFormat -- 'B'
                  | ClosePortal PortalName -- 'C'
                  | ClosePreparedStatement PreparedStatementName -- 'C'
                  | CopyDataIn ByteString -- 'd'
                  | CopyInDone -- 'c'
                  | CopyFail ByteString0 -- 'f'
                  | DescribePortal PortalName -- 'D'
                  | DescribePreparedStatement PreparedStatementName -- 'D'
                  | Execute PortalName Word32 -- 'E'; 0 in the limit field means "no limit"
                  | Flush -- 'H'
                  | FunctionCall Oid BoundParams FormatCode -- 'F'
                  | Parse PreparedStatementName ByteString0 [Oid] -- 'P'
                  | PasswordMessage ByteString0 -- 'p'
                  | GSSPI_or_SSPIResponse ByteString -- 'p' -- yes, overlap with PasswordMessage
                  | Query ByteString0 -- 'Q'
                  | Sync -- 'S'
                  | Terminate -- 'X'
                    deriving (Show, Read, Eq, Ord)

data InitialMessage = SSLRequest
                    | StartupMessage [(ByteString0,ByteString0)]
                    | CancelRequest ServerPid ServerKey
                      deriving (Show, Read, Eq, Ord)

data ProtocolError = PacketTooLarge
                   | ParseError [String] String
                   | UnexpectedMessage FromBackend
                     deriving (Show, Typeable)
instance Exception ProtocolError
