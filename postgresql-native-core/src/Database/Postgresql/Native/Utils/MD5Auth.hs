{-# LANGUAGE OverloadedStrings #-}

module Database.Postgresql.Native.Utils.MD5Auth (
  md5password
) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (encode)
import Data.Monoid
import Data.Digest.MD5 (hash)

import Database.Postgresql.Native.Types

md5password :: ByteString0 -> ByteString0 -> ByteString -> ByteString0
md5password username password salt =
  let md5 = encode . BS.pack . hash . BS.unpack
  in unsafeNewByteString0 ("md5" <> md5 (md5 (toByteString $ password <> username) <> salt))
