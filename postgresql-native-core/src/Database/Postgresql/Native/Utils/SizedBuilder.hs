module Database.Postgresql.Native.Utils.SizedBuilder (
  SizedBuilder,
  length,
  bytestring,
  builder,
  toLazyByteString,
  int32BE,
  int16BE,
  word32BE,
  word16BE,
  word8,
  char8
) where

import Prelude hiding (length)
import Data.Monoid

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.Char (ord)
import Data.Int (Int16, Int32)
import Data.Word (Word8, Word16, Word32)

data SizedBuilder = SizedBuilder !Int !B.Builder

length :: SizedBuilder -> Int
length (SizedBuilder l _) = l

builder :: SizedBuilder -> B.Builder
builder (SizedBuilder _ b) = b

instance Monoid SizedBuilder where
    mempty = SizedBuilder 0 mempty
    mappend (SizedBuilder s1 b1) (SizedBuilder s2 b2) = SizedBuilder (s1+s2) (b1 <> b2)

toLazyByteString :: SizedBuilder -> BSL.ByteString
toLazyByteString (SizedBuilder _ b) = B.toLazyByteString b

bytestring :: BS.ByteString -> SizedBuilder
bytestring bs = SizedBuilder (BS.length bs) (B.byteString bs)

int32BE :: Int32 -> SizedBuilder
int32BE i = SizedBuilder 4 (B.int32BE i)

int16BE :: Int16 -> SizedBuilder
int16BE i = SizedBuilder 2 (B.int16BE i)

word32BE :: Word32 -> SizedBuilder
word32BE i = SizedBuilder 4 (B.word32BE i)

word16BE :: Word16 -> SizedBuilder
word16BE i = SizedBuilder 2 (B.word16BE i)

word8 :: Word8 -> SizedBuilder
word8 b = SizedBuilder 1 (B.word8 b)

char8 :: Char -> SizedBuilder
char8 c = SizedBuilder 1 (B.word8 $ fromIntegral $ ord c)
