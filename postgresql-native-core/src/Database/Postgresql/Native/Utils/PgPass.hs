{-# LANGUAGE CPP, OverloadedStrings, LambdaCase #-}

module Database.Postgresql.Native.Utils.PgPass (
  PermissionFailReason(..)
, ConnectSpec(..)
, parsePgPass
, readPgPass
, readPgPass'
, passwordFor
, pgPassFile
, lookupPassword
) where

-- This should really use Parsec or even a hand-rolled parser instead
-- of Attoparsec for better error reporting.

import Control.Exception (catch, throwIO)
import System.IO.Error (isDoesNotExistError, isPermissionError)
import Data.Char (isSpace)
import Data.Text as T
import Data.Text.IO as T
import Data.Attoparsec.Text as AP
import Control.Applicative
import Data.List as L
import System.FilePath ((</>))
import System.Environment (lookupEnv)

#if PGN_UNIX
import Database.Postgresql.Native.Utils.Unix (getConfigDir)
import Database.Postgresql.Native.Utils.PgPass.Unix
#else
import Database.Postgresql.Native.Utils.Windows (getConfigDir)
import Database.Postgresql.Native.Utils.PgPass.Windows

checkPermissions :: Handle -> IO (Either PermissionFailReason ())
checkPermissions = return $ Right ()
#endif

data ConnectSpec = ConnectSpec { hostname :: Maybe Text
                               , port :: Maybe Int
                               , database :: Maybe Text
                               , username :: Maybe Text
                               , password :: Text
                               } deriving (Read, Show, Eq, Ord)

data Line = Comment | Spec ConnectSpec | Blank
          deriving (Show)

commentParser :: Parser Line
commentParser = skipWhile isLinearSpace *> char '#' *> skipWhile (/= '\n') *> pure Comment

segmentParser :: Parser Text
segmentParser = trim <$> pack <$> many segmentChar
    where segmentChar = do
            c <- satisfy notEndOfSegment
            if c == '\\'
            then anyChar
            else return c
          notEndOfSegment c =
              c /= ':' && c /= '\n' && c /= '\0'
          trim = fst . spanEnd isSpace . T.dropWhile isSpace

spanEnd :: (Char -> Bool) -> Text -> (Text, Text)
spanEnd p t = let (a, b) = T.span p $ T.reverse t
              in (T.reverse b, T.reverse a)

isDigit :: Char -> Bool
isDigit c = '0' <= c && c <= '9'

isLinearSpace :: Char -> Bool
isLinearSpace c = isSpace c && c /= '\n'

blankParser :: Parser Line
blankParser = skipWhile isLinearSpace *> pure Blank

blank1Parser :: Parser Line
blank1Parser = satisfy isLinearSpace *> blankParser

hostNameParser :: Parser (Maybe Text)
hostNameParser = wildcardable segmentParser

userNameParser :: Parser (Maybe Text)
userNameParser = wildcardable segmentParser

databaseParser :: Parser (Maybe Text)
databaseParser = wildcardable segmentParser

passwordParser :: Parser Text
passwordParser = segmentParser

portParser :: Parser (Maybe Int)
portParser = wildcardable $ do
  segment <- segmentParser
  case T.span isDigit segment of
    (digits, "") -> return $ read $ unpack digits
    _ -> fail "Non-digit in port"

wildcardable :: Parser a -> Parser (Maybe a)
wildcardable p = parseWildcard <|> (Just <$> p)
    where parseWildcard = do
            s <- segmentParser
            if s == "*"
            then return Nothing
            else fail "wildcard"

specParser :: Parser ConnectSpec
specParser = ConnectSpec <$> (hostNameParser <* char ':') <*> (portParser <* char ':') <*> (databaseParser <* char ':') <*> (userNameParser <* char ':') <*> passwordParser

lineParser :: Parser Line
lineParser = preEofParser <|> atEofParser <?> "line"
    where preEofParser = (commentParser <|> (Spec <$> specParser) <|> blankParser) <* endOfLine
          atEofParser = (commentParser <|> (Spec <$> specParser) <|> blank1Parser) <* endOfInput

pgPassParser :: Parser [Line]
pgPassParser = many lineParser <* endOfInput

parsePgPass :: Text -> Either String [ConnectSpec]
parsePgPass = fmap extractSpec . parseOnly pgPassParser
    where extractSpec ls = [spec | Spec spec <- ls]

readPgPass' :: FilePath -> IO (Either PermissionFailReason Text)
readPgPass' f = (checkPermissions f >>= \case
                  Left err -> return $ Left err
                  Right () -> Right <$> T.readFile f) `catch` ioErrors
    where ioErrors e =
              if isDoesNotExistError e
              then return $ Left DoesNotExist
              else if isPermissionError e
                   then return $ Left PermissionsTooTight
                   else throwIO e

readPgPass :: FilePath -> IO Text
readPgPass = fmap (either (const "") id) . readPgPass'

passwordFor :: Text -> Int -> Text -> Text -> [ConnectSpec] -> Maybe Text
passwordFor targetHost targetPort targetDatabase targetUser cs = password <$> L.find conforms cs
    where conforms (ConnectSpec h p d u _) = accepts targetHost h && accepts targetPort p && accepts targetDatabase d && accepts targetUser u
          accepts _ Nothing = True
          accepts a (Just b) = a == b

pgPassFile :: IO (Maybe FilePath)
pgPassFile = lookupEnv "PGPASSFILE" >>= \case
               Just f -> return $ Just f
               Nothing -> (fmap.fmap) (</> ".pgpass") getConfigDir

lookupPassword :: Text -> Int -> Text -> Text -> IO (Maybe Text)
lookupPassword h p d u = pgPassFile >>= \case
                           Just file -> readPgPass file >>= \t ->
                             case parsePgPass t of
                               Left _ -> return Nothing
                               Right cs -> return $ passwordFor h p d u cs
                           Nothing ->
                               return Nothing
