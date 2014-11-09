module Database.Postgresql.Native.Types (
  ByteString0
, toByteString
, newByteString0
, unsafeNewByteString0

, ShortByteString0
, toShort0
, fromShort0
, toShortByteString
, newShortByteString0
, unsafeNewShortByteString0

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
, ErrorData
) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Short as SBS
import Data.ByteString.Short (ShortByteString)
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

bsify1 :: (ByteString, String) -> (ByteString0, String)
bsify1 (bs, s) = (newByteString0 bs, s)

bsifyn :: ([ByteString], String) -> ([ByteString0], String)
bsifyn (bss, s) = (map newByteString0 bss, s)

instance Monoid ByteString0 where
    mempty = unsafeNewByteString0 BS.empty
    mappend (ByteString0 a) (ByteString0 b) = unsafeNewByteString0 (a <> b)

instance IsString ByteString0 where
    fromString = newByteString0 . fromString

toByteString :: ByteString0 -> ByteString
toByteString (ByteString0 bs) = bs

unsafeNewByteString0 :: ByteString -> ByteString0
unsafeNewByteString0 bs = ByteString0 bs

newByteString0 :: ByteString -> ByteString0
newByteString0 = unsafeNewByteString0 -- todo check for 0

newtype ShortByteString0 = ShortByteString0 ShortByteString deriving (Eq, Ord)

instance Show ShortByteString0 where
    showsPrec i (ShortByteString0 bs) = showsPrec i bs
    show (ShortByteString0 bs) = show bs
    showList bss = showList $ map (toByteString . fromShort0) bss

instance Read ShortByteString0 where
    readsPrec i = map sbsify1 . readsPrec i
    readList = map sbsifyn . readList

instance IsString ShortByteString0 where
    fromString = newShortByteString0 . fromString

sbsify1 :: (ShortByteString, String) -> (ShortByteString0, String)
sbsify1 (bs, s) = (newShortByteString0 bs, s)

sbsifyn :: ([ShortByteString], String) -> ([ShortByteString0], String)
sbsifyn (bss, s) = (map newShortByteString0 bss, s)

toShortByteString :: ShortByteString0 -> ShortByteString
toShortByteString (ShortByteString0 bs) = bs

unsafeNewShortByteString0 :: ShortByteString -> ShortByteString0
unsafeNewShortByteString0 bs = ShortByteString0 bs

newShortByteString0 :: ShortByteString -> ShortByteString0
newShortByteString0 = unsafeNewShortByteString0 -- todo check for 0

toShort0 :: ByteString0 -> ShortByteString0
toShort0 (ByteString0 bs) = ShortByteString0 $ SBS.toShort bs

fromShort0 :: ShortByteString0 -> ByteString0
fromShort0 (ShortByteString0 sbs) = ByteString0 $ SBS.fromShort sbs

-- | Field codes are in <http://www.postgresql.org/docs/9.4/static/protocol-error-fields.html the manual>
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
                    | UnknownAuthCode Word32 ByteString
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
    , rcColumn :: Oid
    -- ^ Column oid, if the field is a table column, or 0
    , rcColumnAttribute :: Word16
    -- ^ Column attribute number, if the field is a table column, or 0
    , rcType :: Oid
    -- ^ oid of the field's type
    , rcTypeSize :: Int16
    -- ^ data type size (See <http://www.postgresql.org/docs/9.4/static/catalog-pg-type.html pg_type.typelen>)
    , rcTypeModifier :: Word32
    -- ^ type modifier (See <http://www.postgresql.org/docs/9.4/static/catalog-pg-attribute.html pg_attribute.atttypmod>)
    , rcFormatCode :: FormatCode
    } deriving (Show, Read, Eq, Ord)

data BoundParams = UniformatParams FormatCode [Maybe ByteString]
                 -- ^ The list must contain fewer than 2^16 elements.
                 | PolyformatParams [(FormatCode, Maybe ByteString)]
                 -- ^ The list must contain fewer than 2^16 elements.
                   deriving (Eq, Ord, Show, Read)

data ResultFormat = UniformatResult FormatCode
                  | PolyformatResult [FormatCode]
                    -- ^ The list must contain fewer than 2^16 elements.
                    deriving (Eq, Ord, Show, Read)

type ErrorData = [(MessageField, ByteString0)]
