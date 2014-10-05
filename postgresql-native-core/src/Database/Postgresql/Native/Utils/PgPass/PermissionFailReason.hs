module Database.Postgresql.Native.Utils.PgPass.PermissionFailReason (
  PermissionFailReason(..)
) where

data PermissionFailReason = DoesNotExist | NotRegularFile | PermissionsTooLoose | PermissionsTooTight
                            deriving (Eq, Ord, Read, Show)
