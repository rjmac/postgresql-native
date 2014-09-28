module Database.Postgresql.Native.Types (
  ByteString0
, toByteString
, newByteString0
, unsafeNewByteString0

, MessageField
, PortalName(..) -- SC
, PreparedStatementName(..) -- SC
, ChannelName(..) -- SC
, ParameterName(..) -- SC
, FormatCode(..)
, AuthResultCode(..)
, ServerPid(..) -- SC
, ServerKey(..) -- SC
, Oid(..) -- SC
, StatusIndicator(..)
, RowColumnDescription(..)
, BoundParams (..)
, ResultFormat (..)
) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Monoid
import Data.String
import Data.Word (Word16, Word32)
import Data.Int (Int16)

newtype ByteString0 = ByteString0 ByteString deriving (Eq, Ord)

instance Show ByteString0 where
    showsPrec i (ByteString0 bs) = showsPrec i bs
    show (ByteString0 bs) = show bs
    showList bss = showList $ map toByteString bss

instance Read ByteString0 where
    readsPrec i = map bsify1 . readsPrec i
    readList = map bsifyn . readList

instance Monoid ByteString0 where
    mempty = unsafeNewByteString0 BS.empty
    mappend (ByteString0 a) (ByteString0 b) = unsafeNewByteString0 (a <> b)

instance IsString ByteString0 where
    fromString = newByteString0 . fromString

bsify1 :: (ByteString, String) -> (ByteString0, String)
bsify1 (bs, s) = (newByteString0 bs, s)

bsifyn :: ([ByteString], String) -> ([ByteString0], String)
bsifyn (bss, s) = (map newByteString0 bss, s)

toByteString :: ByteString0 -> ByteString
toByteString (ByteString0 bs) = bs

unsafeNewByteString0 :: ByteString -> ByteString0
unsafeNewByteString0 bs = ByteString0 bs

newByteString0 :: ByteString -> ByteString0
newByteString0 = unsafeNewByteString0 -- todo check for 0

type MessageField = Char

newtype PortalName = PortalName ByteString0 deriving (Show, Read, Eq, Ord)
newtype PreparedStatementName = PreparedStatementName ByteString0 deriving (Show, Read, Eq, Ord)
newtype ChannelName = ChannelName ByteString0 deriving (Show, Read, Eq, Ord)
newtype ParameterName = ParameterName ByteString0 deriving (Show, Read, Eq, Ord)

data FormatCode = Text -- 0
                | Binary -- 1
                  deriving (Show, Read, Eq, Ord)

data AuthResultCode = AuthOK -- 0
                    | KerberosV5 -- 2
                    | CleartextPassword -- 3
                    | MD5Password ByteString -- 5
                    | SCMCredential -- 6
                    | GSS -- 7
                    | SSPI -- 9
                    | GSSContinue ByteString -- 8
                      deriving (Show, Read, Eq, Ord)

newtype ServerPid = ServerPid Word32
    deriving (Show, Read, Eq, Ord)
newtype ServerKey = ServerKey Word32
    deriving (Show, Read, Eq, Ord)
newtype Oid = Oid Word32
    deriving (Show, Read, Eq, Ord)

data StatusIndicator = StatusIdle -- 'I'
                     | StatusTransaction -- 'T'
                     | StatusFailedTransaction -- 'E'
                       deriving (Show, Read, Eq, Ord)

-- This is a bit ick
data RowColumnDescription = RowColumnDescription {
      rcFieldName :: ByteString0
    , rcColumn :: Oid             --   Column oid, if the field is a table column, or 0
    , rcColumnAttribute :: Word16 --   Column attribute number, if the field is a table column, or 0
    , rcType :: Oid               --   oid of the field's type
    , rcTypeSize :: Int16         --   data type size -- negative means variable-sized (pg_type.typlen)
    , rcTypeModifier :: Word32    --   type modifier (pg_attribute.atttypmod).  Meaning is type-specific
    , tcFormatCode :: FormatCode
    } deriving (Show, Read, Eq, Ord)

-- The lists here must not be >65535 elements long; their length is
-- encoded as a 16-bit integer.
data BoundParams = UniformatParams FormatCode [Maybe ByteString]
                 | PolyformatParams [(FormatCode, Maybe ByteString)]
                   deriving (Eq, Ord, Show, Read)

-- The list of format codes must not be >65535 elements long; its length
-- is encoded as a 16-bit integer.
data ResultFormat = UniformatResult FormatCode
                  | PolyformatResult [FormatCode]
                    deriving (Eq, Ord, Show, Read)
