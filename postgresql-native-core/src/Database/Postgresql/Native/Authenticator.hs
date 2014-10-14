module Database.Postgresql.Native.Authenticator (
  Authenticator
, defaultAuthenticator
, passwordAuthenticator
) where

import Database.Postgresql.Native.Types(AuthResultCode(..), ByteString0)
import Database.Postgresql.Native.Message(FromFrontend(PasswordMessage))
import Database.Postgresql.Native.Transport(sendMessage, sendSCMCredentials)
import Database.Postgresql.Native.Utils(md5password)
import Database.Postgresql.Native.Authenticator.Internal

-- | An 'Authenticator' which can handle authentication methods that
-- require no additional information (i.e., 'AuthOK' and
-- 'SCMCredential')
defaultAuthenticator :: Authenticator
defaultAuthenticator = Authenticator auth
    where auth AuthOK = Just $ \_ _ -> return ()
          auth SCMCredential = Just $ \t _ -> do
                                 sendSCMCredentials t
                                 expectAuthOK t
          auth _ = Nothing

-- | An 'Authenticator' which can handle authentication methods that
-- require a simple password (i.e., 'CleartextPassword' and
-- 'MD5Password')
passwordAuthenticator :: ByteString0 -> Authenticator
passwordAuthenticator password = Authenticator auth
    where auth CleartextPassword = Just $ \t _ -> do
                                     sendMessage t $ PasswordMessage $ password
                                     expectAuthOK t
          auth (MD5Password salt) = Just $ \t username -> do
                                      sendMessage t $ PasswordMessage $ md5password username password salt
                                      expectAuthOK t
          auth _ = Nothing
