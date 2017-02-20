module Paths_hask_server (
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
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/yoav/.cabal/bin"
libdir     = "/home/yoav/.cabal/lib/hask-server-0.1.0.0/ghc-7.6.3"
datadir    = "/home/yoav/.cabal/share/hask-server-0.1.0.0"
libexecdir = "/home/yoav/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "hask_server_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hask_server_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "hask_server_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hask_server_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
