{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_morse (
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

bindir     = "/Users/yuzhao/gits/haskell-programming-ffp/chap14/morse/.stack-work/install/x86_64-osx/lts-12.13/8.4.3/bin"
libdir     = "/Users/yuzhao/gits/haskell-programming-ffp/chap14/morse/.stack-work/install/x86_64-osx/lts-12.13/8.4.3/lib/x86_64-osx-ghc-8.4.3/morse-0.1.0.0-4u16HIsPnFzJFl1Cyn47g9-morse"
dynlibdir  = "/Users/yuzhao/gits/haskell-programming-ffp/chap14/morse/.stack-work/install/x86_64-osx/lts-12.13/8.4.3/lib/x86_64-osx-ghc-8.4.3"
datadir    = "/Users/yuzhao/gits/haskell-programming-ffp/chap14/morse/.stack-work/install/x86_64-osx/lts-12.13/8.4.3/share/x86_64-osx-ghc-8.4.3/morse-0.1.0.0"
libexecdir = "/Users/yuzhao/gits/haskell-programming-ffp/chap14/morse/.stack-work/install/x86_64-osx/lts-12.13/8.4.3/libexec/x86_64-osx-ghc-8.4.3/morse-0.1.0.0"
sysconfdir = "/Users/yuzhao/gits/haskell-programming-ffp/chap14/morse/.stack-work/install/x86_64-osx/lts-12.13/8.4.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "morse_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "morse_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "morse_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "morse_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "morse_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "morse_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
