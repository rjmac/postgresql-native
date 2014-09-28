module Database.Postgresql.Native.Types.Deserialization where

import Control.Applicative ((<$>),(<*),(*>))
import Data.Attoparsec.ByteString as AP
import qualified Data.Attoparsec.ByteString.Char8 as C8
import Data.Attoparsec.Binary
import Database.Postgresql.Native.Types

bytestring0 :: Parser ByteString0
bytestring0 = unsafeNewByteString0 <$> AP.takeWhile (/= 0) <* word8 0 <?> "bytestring0"

oid :: Parser Oid
oid = Oid <$> anyWord32be

formatCode8 :: Parser FormatCode
formatCode8 = codify <$> satisfy zeroOne <?> "formatCode"
    where zeroOne x = x == 0 || x == 1
          codify 0 = Text
          codify 1 = Binary
          codify _ = error "formatCode"

formatCode16 :: Parser FormatCode
formatCode16 = word8 0 *> formatCode8 <?> "formatCode"

statusIndicator :: Parser StatusIndicator
statusIndicator = statify <$> C8.satisfy si <?> "statusIndicator"
    where si c = c == 'I' || c == 'T' || c == 'E'
          statify 'I' = StatusIdle
          statify 'T' = StatusTransaction
          statify 'E' = StatusFailedTransaction
          statify _ = error "statusIndicator"

messageField :: Parser MessageField
messageField = C8.anyChar <?> "message field" -- TODO: Learn about specific codes

serverPid :: Parser ServerPid
serverPid = ServerPid <$> anyWord32be <?> "server pid"

channelName :: Parser ChannelName
channelName = ChannelName <$> bytestring0 <?> "channel name"

parameterName :: Parser ParameterName
parameterName = ParameterName <$> bytestring0 <?> "parameter name"
