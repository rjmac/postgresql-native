module Database.Postgresql.Native.Types.Serialization (
  bytestring0
, oid
, preparedStatementName
, portalName
, formatCode16
, serverPid
, serverKey
, boundParams
, resultFormat
) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Monoid
import Database.Postgresql.Native.Utils.SizedBuilder
import Database.Postgresql.Native.Types

bytestring0 :: ByteString0 -> SizedBuilder
bytestring0 bs = bytestring (toByteString bs) <> word8 0

oid :: Oid -> SizedBuilder
oid (Oid o) = word32BE o

preparedStatementName :: PreparedStatementName -> SizedBuilder
preparedStatementName (PreparedStatementName bs0) = bytestring0 bs0

portalName :: PortalName -> SizedBuilder
portalName (PortalName bs0) = bytestring0 bs0

formatCode16 :: FormatCode -> SizedBuilder
formatCode16 Text = int16BE 0
formatCode16 Binary = int16BE 1

serverPid :: ServerPid -> SizedBuilder
serverPid (ServerPid p) = word32BE p

serverKey :: ServerKey -> SizedBuilder
serverKey (ServerKey k) = word32BE k

mbytestring :: Maybe ByteString -> SizedBuilder
mbytestring (Just bs) = word32BE (fromIntegral $ BS.length bs) <> bytestring bs
mbytestring Nothing = word32BE (-1)

params :: [Maybe ByteString] -> SizedBuilder
params ps = word16BE (fromIntegral $ Prelude.length ps) <> mconcat (map mbytestring ps)

formatCodes :: [FormatCode] -> SizedBuilder
formatCodes fcs = word16BE (fromIntegral $ Prelude.length fcs) <> mconcat (map formatCode16 fcs)

boundParams :: BoundParams -> SizedBuilder
boundParams (UniformatParams Text ps) = word16BE 0 <> params ps
boundParams (UniformatParams notText ps) = word16BE 1 <> formatCode16 notText <> params ps
boundParams (PolyformatParams fcps) = formatCodes (map fst fcps) <> params (map snd fcps)

resultFormat :: ResultFormat -> SizedBuilder
resultFormat (UniformatResult Text) = word16BE 0
resultFormat (UniformatResult notText) = word16BE 1 <> formatCode16 notText
resultFormat (PolyformatResult fcs) = formatCodes fcs
