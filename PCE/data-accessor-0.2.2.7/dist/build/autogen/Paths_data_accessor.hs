module Paths_data_accessor (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,2,2,7], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/data-accessor-0.2.2.7/ghc-7.6.3"
datadir    = "/usr/local/share/data-accessor-0.2.2.7"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "data_accessor_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "data_accessor_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "data_accessor_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "data_accessor_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
