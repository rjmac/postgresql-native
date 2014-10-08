module Database.Postgresql.Native.Utils.PgPass.Windows (
  checkPermissions,
  PermissionFailReason(..)
) where

import Database.Postgresql.Native.Utils.PgPass.PermissionFailReason

checkPermissions :: FilePath -> IO (Either PermissionFailReason ())
checkPermissions = return $ Right ()
