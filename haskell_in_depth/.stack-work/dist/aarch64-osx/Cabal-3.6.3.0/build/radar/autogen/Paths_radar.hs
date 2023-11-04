{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_radar (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/kevin/dev/haskell/haskell_in_depth/.stack-work/install/aarch64-osx/10b7724d80ef68265e84645f266c97e67a35a3ea8627a6ff4960800ba7b3b260/9.2.8/bin"
libdir     = "/Users/kevin/dev/haskell/haskell_in_depth/.stack-work/install/aarch64-osx/10b7724d80ef68265e84645f266c97e67a35a3ea8627a6ff4960800ba7b3b260/9.2.8/lib/aarch64-osx-ghc-9.2.8/radar-0.1.0.0-85CwHfNmr4oJaKyN2mceVp-radar"
dynlibdir  = "/Users/kevin/dev/haskell/haskell_in_depth/.stack-work/install/aarch64-osx/10b7724d80ef68265e84645f266c97e67a35a3ea8627a6ff4960800ba7b3b260/9.2.8/lib/aarch64-osx-ghc-9.2.8"
datadir    = "/Users/kevin/dev/haskell/haskell_in_depth/.stack-work/install/aarch64-osx/10b7724d80ef68265e84645f266c97e67a35a3ea8627a6ff4960800ba7b3b260/9.2.8/share/aarch64-osx-ghc-9.2.8/radar-0.1.0.0"
libexecdir = "/Users/kevin/dev/haskell/haskell_in_depth/.stack-work/install/aarch64-osx/10b7724d80ef68265e84645f266c97e67a35a3ea8627a6ff4960800ba7b3b260/9.2.8/libexec/aarch64-osx-ghc-9.2.8/radar-0.1.0.0"
sysconfdir = "/Users/kevin/dev/haskell/haskell_in_depth/.stack-work/install/aarch64-osx/10b7724d80ef68265e84645f266c97e67a35a3ea8627a6ff4960800ba7b3b260/9.2.8/etc"

getBinDir     = catchIO (getEnv "radar_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "radar_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "radar_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "radar_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "radar_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "radar_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
