{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_hide (
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
bindir     = "/Users/kevin/.cabal/bin"
libdir     = "/Users/kevin/.cabal/lib/aarch64-osx-ghc-9.2.8/hide-0.1.0.0-inplace-hide"
dynlibdir  = "/Users/kevin/.cabal/lib/aarch64-osx-ghc-9.2.8"
datadir    = "/Users/kevin/.cabal/share/aarch64-osx-ghc-9.2.8/hide-0.1.0.0"
libexecdir = "/Users/kevin/.cabal/libexec/aarch64-osx-ghc-9.2.8/hide-0.1.0.0"
sysconfdir = "/Users/kevin/.cabal/etc"

getBinDir     = catchIO (getEnv "hide_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "hide_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "hide_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "hide_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hide_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hide_sysconfdir") (\_ -> return sysconfdir)



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
