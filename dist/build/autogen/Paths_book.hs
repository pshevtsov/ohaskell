module Paths_book (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/dshevchenko/Library/Haskell/bin"
libdir     = "/Users/dshevchenko/Library/Haskell/ghc-7.8.3-x86_64/lib/book-0.1.0.0"
datadir    = "/Users/dshevchenko/Library/Haskell/share/ghc-7.8.3-x86_64/book-0.1.0.0"
libexecdir = "/Users/dshevchenko/Library/Haskell/libexec"
sysconfdir = "/Users/dshevchenko/Library/Haskell/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "book_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "book_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "book_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "book_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "book_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
