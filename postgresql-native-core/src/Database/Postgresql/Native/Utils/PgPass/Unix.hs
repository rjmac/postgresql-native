module Database.Postgresql.Native.Utils.PgPass.Unix (
  checkPermissions,
  PermissionFailReason(..)
) where

import System.Posix.Files (getFileStatus, isRegularFile, fileMode)
import Data.Bits ((.&.))

import Database.Postgresql.Native.Utils.PgPass.PermissionFailReason

checkPermissions :: FilePath -> IO (Either PermissionFailReason ())
checkPermissions f = do
  fs <- getFileStatus f
  return $ if isRegularFile fs
           then if fileMode fs .&. 0o077 == 0
                then Right ()
                else Left PermissionsTooLoose
           else Left NotRegularFile
