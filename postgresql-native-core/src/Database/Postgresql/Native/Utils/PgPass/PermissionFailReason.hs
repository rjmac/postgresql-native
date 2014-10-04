module Database.Postgresql.Native.Utils.PgPass.PermissionFailReason (
  PermissionFailReason(..)
) where

data PermissionFailReason = NotRegularFile | PermissionsTooLoose
                            deriving (Eq, Ord, Read, Show)
