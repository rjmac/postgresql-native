module Database.Postgresql.Native.Utils.Windows (
  getConfigDir
) where

import Control.Applicative ((<$>))
import System.FilePath ((</>))
import System.Win32

getConfigDir = (</> "postgresql") <$> appdir

appdir :: IO FilePath
appdir = sHGetFolderPath nullPtr cSIDL_APPDATA nullPtr sHGFP_TYPE_CURRENT
