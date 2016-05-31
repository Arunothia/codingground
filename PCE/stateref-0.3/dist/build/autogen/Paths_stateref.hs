module Paths_stateref (
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
version = Version {versionBranch = [0,3], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/stateref-0.3/ghc-7.6.3"
datadir    = "/usr/local/share/stateref-0.3"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "stateref_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "stateref_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "stateref_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "stateref_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
