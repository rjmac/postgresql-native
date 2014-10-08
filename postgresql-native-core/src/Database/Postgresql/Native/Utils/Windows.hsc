module Database.Postgresql.Native.Utils.Windows (
  getConfigDir
) where

import Foreign
import Foreign.C
import System.FilePath ((</>))

#include <Windows.h>

getConfigDir = (fmap.fmap) (</> "postgresql") appdir

appdir :: IO (Maybe FilePath)
appdir = allocaArray #{const MAX_PATH} $ \pathbuf ->
           r <- c_SHGetFolderPath nullPtr #{const CSIDL_APPDATA} nullPtr 0 pathBuf
           if r == #{const S_OK}
           then Just <$> peekCWString pathbuf
           else return Nothing

foreign import stdcall unsafe "shlobj.h SHGetFolderPathW"
c_SHGetFolderPath :: Ptr () -> CInt -> Ptr () -> CInt -> CWString -> IO CInt
