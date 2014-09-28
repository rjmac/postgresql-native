module Database.Postgresql.Native.Connection.Tracing (
  trace
) where

import Database.Postgresql.Native.Connection
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Numeric (showHex)
import Data.Word (Word32)
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Data.List (intercalate)
import Data.Char (chr)

trace :: (String -> IO ()) -> Connection -> IO Connection
trace out conn = do
  inCtr <- newIORef 0
  outCtr <- newIORef 0
  return $ Connection {
                   send = sendTrace out inCtr (send conn)
                 , recv = recvTrace out outCtr (recv conn)
                 , closeNicely = closeNicelyTrace out (closeNicely conn)
                 , closeRudely = closeRudelyTrace out (closeRudely conn)
                 , makeSSL = fmap (makeSSLTrace out) (makeSSL conn)
                 , sendSCMCredentials = fmap (sendSCMCredentialsTrace out) (sendSCMCredentials conn)
                 }

outbound, inbound :: String
outbound = "<--  "
inbound = "-->  "

sendTrace :: (String -> IO ()) -> IORef Word32 -> (BSL.ByteString -> IO ()) -> BSL.ByteString -> IO ()
sendTrace out ctr orig bs = do
  start <- readIORef ctr
  mapM_ out $ hexTrace outbound start bs
  writeIORef ctr (start + fromIntegral (BSL.length bs))
  orig bs

recvTrace :: (String -> IO ()) -> IORef Word32 -> (Int -> IO BS.ByteString) -> Int -> IO BS.ByteString
recvTrace out ctr orig n = do
  bs <- orig n
  start <- readIORef ctr
  mapM_ out $ hexTrace inbound start (BSL.fromStrict bs)
  writeIORef ctr (start + fromIntegral (BS.length bs))
  return bs

hexTrace :: String -> Word32 -> BSL.ByteString -> [String]
hexTrace pfx start bs = map ((pfx ++) . uncurry hexLine) (chunks start bs)

hex :: (Show n, Integral n) => Int -> n -> String
hex l i = let h = showHex i ""
              n = l - length h
              p = replicate n '0'
          in p ++ h

halfHexLineLength, hexLineLength :: (Integral a) => a
halfHexLineLength = 8
hexLineLength = 16

chunks :: Word32 -> BSL.ByteString -> [(Word32, BSL.ByteString)]
chunks start bs =
    if BSL.null bs
    then []
    else let offset = start `mod` hexLineLength
             end = (start - offset) + hexLineLength
             lineLength = fromIntegral (end - start)
         in (start, BSL.take lineLength bs) : chunks (start + fromIntegral lineLength) (BSL.drop lineLength bs)

hexLine :: Word32 -> BSL.ByteString -> String
hexLine start chunk = hex 8 start ++ "  " ++ visibleBytes ++ "  |" ++ visibleChars ++ "|"
    where offset = fromIntegral $ start `mod` hexLineLength
          end = (fromIntegral $ start - fromIntegral offset) + hexLineLength
          padLeft = replicate offset "  "
          padRightLength = end - fromIntegral (fromIntegral (BSL.length chunk) + start)
          padRight = replicate padRightLength "  "
          bytesRaw = BSL.unpack chunk
          visibleBytesRaw = padLeft ++ map (hex 2) bytesRaw ++ padRight
          visibleBytes = intercalate " " (take halfHexLineLength visibleBytesRaw) ++ " - " ++ intercalate " " (drop halfHexLineLength visibleBytesRaw)
          charsPadLeft = replicate offset ' '
          charsPadRight = replicate padRightLength ' '
          visibleCharsRaw = map toChar bytesRaw
          visibleChars = charsPadLeft ++ visibleCharsRaw ++ charsPadRight
          toChar b = if b < 33 || b > 126
                     then '\xb7'
                     else chr $ fromIntegral b

closeNicelyTrace :: (String -> IO ()) -> IO () -> IO ()
closeNicelyTrace out orig = do
  out $ outbound ++ "Closing nicely"
  orig

closeRudelyTrace :: (String -> IO ()) -> IO () -> IO ()
closeRudelyTrace out orig = do
  out $ outbound ++ "Closing rudely"
  orig

makeSSLTrace :: (String -> IO ()) -> IO () -> IO ()
makeSSLTrace out orig = do
  out $ outbound ++ "Upgrading to SSL"
  orig

sendSCMCredentialsTrace :: (String -> IO ()) -> IO () -> IO ()
sendSCMCredentialsTrace out orig = do
  out $ outbound ++ "Sending credentials via cmsg"
  orig
