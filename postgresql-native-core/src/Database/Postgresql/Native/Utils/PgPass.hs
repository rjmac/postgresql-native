{-# LANGUAGE CPP, OverloadedStrings, LambdaCase #-}

module Database.Postgresql.Native.Utils.PgPass (
  PermissionFailReason(..)
, ConnectSpec(..)
, readPgPass
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

#ifdef PGN_HAVE_STAT
import Database.Postgresql.Native.Utils.PgPass.Unix
#else
import Database.Postgresql.Native.Utils.PgPass.PermissionFailReason

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

parseComment :: Parser Line
parseComment = skipWhile isLinearSpace *> char '#' *> skipWhile (/= '\n') *> pure Comment

parseSegment :: Parser Text
parseSegment = trim <$> pack <$> many segmentChar
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

parseBlank :: Parser Line
parseBlank = skipWhile isLinearSpace *> pure Blank

parseBlank1 :: Parser Line
parseBlank1 = satisfy isLinearSpace *> parseBlank

parseHostName :: Parser (Maybe Text)
parseHostName = wildcardable parseSegment

parseUserName :: Parser (Maybe Text)
parseUserName = wildcardable parseSegment

parseDatabase :: Parser (Maybe Text)
parseDatabase = wildcardable parseSegment

parsePassword :: Parser Text
parsePassword = parseSegment

parsePort :: Parser (Maybe Int)
parsePort = wildcardable $ do
  segment <- parseSegment
  case T.span isDigit segment of
    (digits, "") -> return $ read $ unpack digits
    _ -> fail "Non-digit in port"

wildcardable :: Parser a -> Parser (Maybe a)
wildcardable p = parseWildcard <|> (Just <$> p)
    where parseWildcard = do
            s <- parseSegment
            if s == "*"
            then return Nothing
            else fail "wildcard"

parseSpec :: Parser ConnectSpec
parseSpec = ConnectSpec <$> (parseHostName <* char ':') <*> (parsePort <* char ':') <*> (parseDatabase <* char ':') <*> (parseUserName <* char ':') <*> parsePassword

parseLine :: Parser Line
parseLine = parsePreEof <|> parseAtEof <?> "line"
    where parsePreEof = (parseComment <|> (Spec <$> parseSpec) <|> parseBlank) <* endOfLine
          parseAtEof = (parseComment <|> (Spec <$> parseSpec) <|> parseBlank1) <* endOfInput

parsePgPass :: Parser [Line]
parsePgPass = many parseLine <* endOfInput

readPgPass :: FilePath -> IO (Either PermissionFailReason (Either String [ConnectSpec]))
readPgPass f = (checkPermissions f >>= \case
                  Left err -> return $ Left err
                  Right () ->
                      Right <$> fmap extractSpec <$> parseOnly parsePgPass <$> T.readFile f
                          where extractSpec ls = [spec | Spec spec <- ls]) `catch` ioErrors
    where ioErrors e =
              if isDoesNotExistError e
              then return $ Left DoesNotExist
              else if isPermissionError e
                   then return $ Left PermissionsTooTight
                   else throwIO e
