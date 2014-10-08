module Database.Postgresql.Native.Utils.Unix (
  getConfigDir
) where

import System.Posix.User (getEffectiveUserID, getUserEntryForID, homeDirectory)
import Control.Applicative ((<$>))
import Control.Exception (catch, throwIO)
import System.IO.Error (isDoesNotExistError)

getConfigDir :: IO (Maybe FilePath)
getConfigDir = dir `catch` handle
    where dir = Just <$> homeDirectory <$> (getEffectiveUserID >>= getUserEntryForID)
          handle e = if isDoesNotExistError e then return Nothing else throwIO e
