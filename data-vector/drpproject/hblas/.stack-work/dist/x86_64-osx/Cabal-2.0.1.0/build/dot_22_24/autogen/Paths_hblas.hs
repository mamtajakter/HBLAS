{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_hblas (
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
version = Version [0,0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/mamtajakter/Documents/COURSES-UO-MS/THESIS/thesis-work/data-vector/drpproject/hblas/.stack-work/install/x86_64-osx/lts-11.11/8.2.2/bin"
libdir     = "/Users/mamtajakter/Documents/COURSES-UO-MS/THESIS/thesis-work/data-vector/drpproject/hblas/.stack-work/install/x86_64-osx/lts-11.11/8.2.2/lib/x86_64-osx-ghc-8.2.2/hblas-0.0.0.0-5zphb53t33kH2zCTfTxl6d-dot_22_24"
dynlibdir  = "/Users/mamtajakter/Documents/COURSES-UO-MS/THESIS/thesis-work/data-vector/drpproject/hblas/.stack-work/install/x86_64-osx/lts-11.11/8.2.2/lib/x86_64-osx-ghc-8.2.2"
datadir    = "/Users/mamtajakter/Documents/COURSES-UO-MS/THESIS/thesis-work/data-vector/drpproject/hblas/.stack-work/install/x86_64-osx/lts-11.11/8.2.2/share/x86_64-osx-ghc-8.2.2/hblas-0.0.0.0"
libexecdir = "/Users/mamtajakter/Documents/COURSES-UO-MS/THESIS/thesis-work/data-vector/drpproject/hblas/.stack-work/install/x86_64-osx/lts-11.11/8.2.2/libexec/x86_64-osx-ghc-8.2.2/hblas-0.0.0.0"
sysconfdir = "/Users/mamtajakter/Documents/COURSES-UO-MS/THESIS/thesis-work/data-vector/drpproject/hblas/.stack-work/install/x86_64-osx/lts-11.11/8.2.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hblas_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hblas_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hblas_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hblas_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hblas_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hblas_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
