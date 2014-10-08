module Database.Postgresql.Native.Utils.Unix (
  getConfigDir
) where

import System.Posix.User (getEffectiveUserID, getUserEntryForID, homeDirectory)
import Control.Applicative ((<$>))

getConfigDir :: IO FilePath
getConfigDir = homeDirectory <$> (getEffectiveUserID >>= getUserEntryForID)
