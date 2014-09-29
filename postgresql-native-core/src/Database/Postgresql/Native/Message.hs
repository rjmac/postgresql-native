-- | <http://www.postgresql.org/docs/9.4/static/protocol-message-formats.html>
module Database.Postgresql.Native.Message where

import Data.ByteString (ByteString)
import Data.Word (Word32)

import Database.Postgresql.Native.Types

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
                 | ErrorResponse [(MessageField, ByteString0)] -- 'E'
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

-- | A pseudomessage sent in response to 'SSLRequest'
data SSLResponse = SSLOK | SSLNotOK | SSLFatal deriving (Eq, Ord, Show, Read)

data FromFrontend = Bind PortalName PreparedStatementName BoundParams ResultFormat -- 'B'
                  | ClosePortal PortalName -- 'C'
                  | ClosePreparedStatement PreparedStatementName -- 'C'
                  | CopyDataIn ByteString -- 'd'
                  | CopyInDone -- 'c'
                  | CopyFail ByteString0 -- 'f'
                  | DescribePortal PortalName -- 'D'
                  | DescribePreparedStatement PreparedStatementName -- 'D'
                  | Execute PortalName Word32 -- 'E'
                    -- ^ 0 in the number field means "no limit"
                  | Flush -- 'H'
                  | FunctionCall Oid BoundParams FormatCode -- 'F'
                  | Parse PreparedStatementName ByteString0 [Oid] -- 'P'
                    -- ^ The list of 'Oid's must contain fewer than 2^16 elements.
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
