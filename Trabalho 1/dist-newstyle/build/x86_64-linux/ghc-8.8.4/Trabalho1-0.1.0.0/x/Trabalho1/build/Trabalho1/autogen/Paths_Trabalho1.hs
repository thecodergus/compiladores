{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Trabalho1 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/gus/.cabal/bin"
libdir     = "/home/gus/.cabal/lib/x86_64-linux-ghc-8.8.4/Trabalho1-0.1.0.0-inplace-Trabalho1"
dynlibdir  = "/home/gus/.cabal/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/gus/.cabal/share/x86_64-linux-ghc-8.8.4/Trabalho1-0.1.0.0"
libexecdir = "/home/gus/.cabal/libexec/x86_64-linux-ghc-8.8.4/Trabalho1-0.1.0.0"
sysconfdir = "/home/gus/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Trabalho1_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Trabalho1_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Trabalho1_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Trabalho1_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Trabalho1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Trabalho1_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
