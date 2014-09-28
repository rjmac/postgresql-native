{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module Database.Postgresql.Native.Message.Serialization (
  serialize
, serializeInitial
, build
, buildInitial
) where

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid

import Database.Postgresql.Native.Utils.SizedBuilder as SB
import Database.Postgresql.Native.Message (FromFrontend(..), InitialMessage(..))

import Database.Postgresql.Native.Types.Serialization

serialize :: FromFrontend -> BSL.ByteString
serialize = B.toLazyByteString . build

serializeInitial :: InitialMessage -> BSL.ByteString
serializeInitial = B.toLazyByteString . buildInitial

build :: FromFrontend -> B.Builder
build = builder . uncurry msg . serializeMessage

buildInitial :: InitialMessage -> B.Builder
buildInitial = builder . initMsg . serializeInitialMessage

msg :: Char -> SizedBuilder -> SizedBuilder
msg c sb = char8 c <> word32BE (4 + fromIntegral (SB.length sb)) <> sb

initMsg :: SizedBuilder -> SizedBuilder
initMsg sb = word32BE (4 + fromIntegral (SB.length sb)) <> sb

serializeMessage :: FromFrontend -> (Char, SizedBuilder)
serializeMessage (Bind pn psn params resfs) = ('B', portalName pn <> preparedStatementName psn <> boundParams params <> resultFormat resfs)
serializeMessage (ClosePreparedStatement psn) = ('C', char8 'S' <> preparedStatementName psn)
serializeMessage (ClosePortal pn) = ('C', char8 'P' <> portalName pn)
serializeMessage (CopyDataIn bs) = ('d', bytestring bs)
serializeMessage CopyInDone = ('c', mempty)
serializeMessage (CopyFail bs0) = ('f', bytestring0 bs0)
serializeMessage (DescribePortal pn) = ('D', char8 'P' <> portalName pn)
serializeMessage (DescribePreparedStatement psn) = ('D', char8 'S' <> preparedStatementName psn)
serializeMessage (Execute pn n) = ('E', portalName pn <> word32BE n)
serializeMessage Flush = ('H', mempty)
serializeMessage (FunctionCall fOid params fc) = ('F', oid fOid <> boundParams params <> formatCode16 fc)
serializeMessage (Parse psn bs0 oids) = ('P', preparedStatementName psn <> bytestring0 bs0 <> mconcat (map oid oids))
serializeMessage (PasswordMessage bs0) = ('p', bytestring0 bs0)
serializeMessage (GSSPI_or_SSPIResponse bs) = ('p', bytestring bs)
serializeMessage (Query bs0) = ('Q', bytestring0 bs0)
serializeMessage Sync = ('S', mempty)
serializeMessage Terminate = ('X', mempty)

serializeInitialMessage :: InitialMessage -> SizedBuilder
serializeInitialMessage SSLRequest = int32BE 80877103
serializeInitialMessage (StartupMessage opts) = int32BE 196608 <> mconcat (map opt opts) <> word8 0 -- 196608 is an encoding of the protocol version
    where opt (k,v) = bytestring0 k <> bytestring0 v
serializeInitialMessage (CancelRequest p k) = int32BE 80877102 <> serverPid p <> serverKey k
