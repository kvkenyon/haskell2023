{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_quilt (
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
version = Version [1,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/kevin/dev/haskell/quilt/.stack-work/install/aarch64-osx/42916ea885dc929f586b731d0a5189bd30a8b65a1fac2e1524a437a3dea27c56/9.4.7/bin"
libdir     = "/Users/kevin/dev/haskell/quilt/.stack-work/install/aarch64-osx/42916ea885dc929f586b731d0a5189bd30a8b65a1fac2e1524a437a3dea27c56/9.4.7/lib/aarch64-osx-ghc-9.4.7/quilt-1.0-INyfb8fIt22BTE51snVoFn-quilt"
dynlibdir  = "/Users/kevin/dev/haskell/quilt/.stack-work/install/aarch64-osx/42916ea885dc929f586b731d0a5189bd30a8b65a1fac2e1524a437a3dea27c56/9.4.7/lib/aarch64-osx-ghc-9.4.7"
datadir    = "/Users/kevin/dev/haskell/quilt/.stack-work/install/aarch64-osx/42916ea885dc929f586b731d0a5189bd30a8b65a1fac2e1524a437a3dea27c56/9.4.7/share/aarch64-osx-ghc-9.4.7/quilt-1.0"
libexecdir = "/Users/kevin/dev/haskell/quilt/.stack-work/install/aarch64-osx/42916ea885dc929f586b731d0a5189bd30a8b65a1fac2e1524a437a3dea27c56/9.4.7/libexec/aarch64-osx-ghc-9.4.7/quilt-1.0"
sysconfdir = "/Users/kevin/dev/haskell/quilt/.stack-work/install/aarch64-osx/42916ea885dc929f586b731d0a5189bd30a8b65a1fac2e1524a437a3dea27c56/9.4.7/etc"

getBinDir     = catchIO (getEnv "quilt_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "quilt_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "quilt_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "quilt_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "quilt_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "quilt_sysconfdir") (\_ -> return sysconfdir)




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
