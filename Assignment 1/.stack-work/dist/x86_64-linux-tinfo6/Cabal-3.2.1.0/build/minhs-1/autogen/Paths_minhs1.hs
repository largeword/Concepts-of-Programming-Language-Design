{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_minhs1 (
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

bindir     = "/home/luxinliang/minhs/.stack-work/install/x86_64-linux-tinfo6/0f5bb029791d331e2cfeefaec3064be500553165b030fe596cb60270ff4b8d9c/8.10.7/bin"
libdir     = "/home/luxinliang/minhs/.stack-work/install/x86_64-linux-tinfo6/0f5bb029791d331e2cfeefaec3064be500553165b030fe596cb60270ff4b8d9c/8.10.7/lib/x86_64-linux-ghc-8.10.7/minhs1-0.1.0.0-G3xUWGt53wXQCKrvkf10i-minhs-1"
dynlibdir  = "/home/luxinliang/minhs/.stack-work/install/x86_64-linux-tinfo6/0f5bb029791d331e2cfeefaec3064be500553165b030fe596cb60270ff4b8d9c/8.10.7/lib/x86_64-linux-ghc-8.10.7"
datadir    = "/home/luxinliang/minhs/.stack-work/install/x86_64-linux-tinfo6/0f5bb029791d331e2cfeefaec3064be500553165b030fe596cb60270ff4b8d9c/8.10.7/share/x86_64-linux-ghc-8.10.7/minhs1-0.1.0.0"
libexecdir = "/home/luxinliang/minhs/.stack-work/install/x86_64-linux-tinfo6/0f5bb029791d331e2cfeefaec3064be500553165b030fe596cb60270ff4b8d9c/8.10.7/libexec/x86_64-linux-ghc-8.10.7/minhs1-0.1.0.0"
sysconfdir = "/home/luxinliang/minhs/.stack-work/install/x86_64-linux-tinfo6/0f5bb029791d331e2cfeefaec3064be500553165b030fe596cb60270ff4b8d9c/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "minhs1_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "minhs1_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "minhs1_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "minhs1_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "minhs1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "minhs1_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
